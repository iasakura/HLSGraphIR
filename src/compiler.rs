use crate::types::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::iter;

use log::{debug, error, log_enabled, info, Level};

const FINISH_STATE : &'static str = "__FINISH";

fn gen_verilog_io_signals(localparams: &Vec<Var>, returns: &Vec<Var>) -> Vec<(VVar, IOType)> {
    let ctrl_sigs = vec!["clk", "rst_n", "start", "finish"];
    let var_list_of_ctrls = ctrl_sigs.iter().map(|var| {
        (VVar {name: String::from(*var), bits: 1, idx: None}, IOType::Input)
    });
    let var_list_of_params = localparams.iter().map(|var| {
        (VVar {name: var.name.clone(), bits: 32, idx: None}, IOType::Input)
    });
    let var_list_of_returns = returns.iter().map(|var| {
        (VVar {name: var.name.clone(), bits: 32, idx: None}, IOType::OutputReg)
    });
    var_list_of_ctrls.chain(var_list_of_params).chain(var_list_of_returns).collect::<Vec<_>>()
}

struct CompilerState {
    localparams: Vec<(VVar, i32)>,
    regs: Vec<VVar>,
    wires : Vec<VAssign>,
    always: Vec<VAlways>,

    cur_state: VVar,
    prev_state: VVar,

    ens: HashMap<Label, VVar>,
    dones: HashMap<Label, VVar>,
    states: HashMap<Label, VExpr>,
}

fn collect_vars<'a, SCHED, II>(dfgbb: &'a DFGBB<SCHED, II>) -> Vec<&'a Var> {
    let dfg = match &dfgbb.body {
        DFGBBBody::Seq(dfg) => dfg,
        DFGBBBody::Pipe(dfg, _) => dfg,
    };
    dfg.iter().map(|(v, _)| v).collect::<Vec<_>>()
}

impl CompilerState {
    fn init(ir: &SchedCDFGIR) -> CompilerState {
        // Create initial compiler state. Don't forget to update cur_state/prev_state!
        let mut cs = CompilerState { 
            localparams: vec![], regs: vec![], wires: vec![], always: vec![],
            cur_state: VVar {name: String::from("dummy"), bits: 0, idx: None},
            prev_state: VVar {name: String::from("dummy"), bits: 0, idx: None},
            ens: HashMap::new(), dones: HashMap::new(), states: HashMap::new(),
        };

        // TODO: bitwidth
        cs.cur_state = cs.new_reg("cur_state", 32, None);
        cs.prev_state = cs.new_reg("prev_state", 32, None);

        // Create states for all CFG block & a final states
        let finish_state = FINISH_STATE.to_string();
        let labels = ir.cdfg.iter().map(|(l, _)| l).chain(iter::once(&finish_state)).collect::<Vec<_>>();
        {
            let mut cnt = 0;
            for &l in &labels {
                let l_en = cs.new_reg(&format!("{}_en", l), 1, None);
                cs.ens.insert(l.clone(), l_en.clone());

                let l_done = cs.new_reg(&format!("{}_done", l), 1, None);
                cs.dones.insert(l.clone(), l_done.clone());

                let var = cs.new_localparam(&format!("{}_ST", l), cnt);
                cs.states.insert(l.clone(), VExpr::Var(var));
                cnt += 1;
            }
        }
        // Create all registers for all SSA variables
        for (_l, dfgbb) in &ir.cdfg {
            let vars = collect_vars(&dfgbb);
            debug!("defined vars of {} = {:?}", _l, vars);
            for v in vars {
                cs.new_reg(&v.name, 32, None);
            }
        }

        cs
    }

    fn new_localparam(&mut self, name: &str, v: i32) -> VVar {
        let var = VVar {name: String::from(name), bits: 32, idx: None};
        self.localparams.push((var, v));
        self.localparams.last().unwrap().0.clone()
    }

    fn new_reg(&mut self, name: &str, bits: i32, idx: Option<i32>) -> VVar {
        if log_enabled!(Level::Debug) {
            if let Some(_) =  self.regs.iter().find(|x| x.name == name) {
                panic!("{} is already defined.\n", name)
            }
        }
        let v = VVar {name: String::from(name), bits, idx};
        self.regs.push(v);
        self.regs.last().unwrap().clone()
    }

    fn new_wire(&mut self, name: &str, bits: i32, idx: Option<i32>, expr: VExpr) -> VVar {
        let v = VVar {name: String::from(name), bits, idx};
        self.wires.push(VAssign { lhs: v, rhs: expr.clone() });
        self.wires.last().unwrap().lhs.clone()
    }

    fn add_event(&mut self, cond: VExpr, assigns: Vec<VAssign>) {
        let clk = VVar {name: String::from("clk"), bits: 1, idx: None};
        self.always.push(VAlways {clk, cond, assigns})
    }
}

fn get<'a, K : std::hash::Hash + std::cmp::Eq + std::fmt::Debug, V>(map: &'a HashMap<K, V>, key: &K) -> &'a V {
    map.get(key).expect(&format!("{:?} is not found.\n", key))
}

fn reduce_exprs(es: &Vec<VExpr>, op: BinOp) -> Option<VExpr> {
    if es.len() == 0 {
        None
    } else {
        let mut ret = es[0].clone();
        for e in &es[1..] {
            ret = VExpr::BinExp(op, Rc::new(ret), Rc::new(e.clone()));
        }
        Some(ret)
    }
}

fn all_true(es: &Vec<VExpr>) -> VExpr {
    reduce_exprs(es, BinOp::And).unwrap()
}

fn ir_var_to_vvar(var: &Var) -> VVar {
    VVar {name: var.name.clone(), bits: 32, idx: None}
}

fn ir_arg_to_vexpr(a: &Arg) -> VExpr {
    match a {
        Arg::Var(v) => VExpr::Var(ir_var_to_vvar(v)),
        Arg::Val(n) => VExpr::Const(*n)
    }
}

fn ir_expr_to_vexpr(e: &Expr, is_first: Option<&VVar>, prevs: &Vec<Label>, cs: &CompilerState) -> VExpr {
    match e {
        Expr::UnExp(op, a) => VExpr::UnExp(*op, Rc::new(ir_arg_to_vexpr(a))),
        Expr::BinExp(op, a1, a2) => {
            match op {
                BinOp::Mu => {
                    if let Some(is_first) = is_first {
                        VExpr::TerExp(TerOp::Select,
                            Rc::new(VExpr::Var(is_first.clone())),
                            Rc::new(ir_arg_to_vexpr(a1)),
                            Rc::new(ir_arg_to_vexpr(a2)),
                        )
                    } else {
                        panic!("Mu requires is_first");
                    }
                    
                }
                BinOp::Ita => {
                    if prevs.len() != 2 {
                        panic!("The number of prev node must be 2 for using Ita.\n");
                    }
                    let t_label = &prevs[0];
                    let t_label_value = get(&cs.states, &t_label);
                    VExpr::TerExp(TerOp::Select, 
                        Rc::new(VExpr::BinExp(
                            BinOp::EQ,
                            // TODO: bitwidth?
                            Rc::new(VExpr::Var(VVar {name: String::from("prev_state"), bits: 32, idx: None})), 
                            Rc::new(t_label_value.clone())
                        )),
                        Rc::new(ir_arg_to_vexpr(a1)),
                        Rc::new(ir_arg_to_vexpr(a2)))
                }
                _ => VExpr::BinExp(*op, Rc::new(ir_arg_to_vexpr(a1)), Rc::new(ir_arg_to_vexpr(a2))),
            }
        },
        Expr::TerExp(op, a1, a2, a3) => VExpr::TerExp(*op, Rc::new(ir_arg_to_vexpr(a1)), Rc::new(ir_arg_to_vexpr(a2)), Rc::new(ir_arg_to_vexpr(a3))),
        Expr::Copy(Arg::Val(n)) => VExpr::Const(*n),
        Expr::Copy(Arg::Var(v)) => VExpr::Var(ir_var_to_vvar(v))
    }
}

fn make_rst_n() -> VVar {
    VVar {name: String::from("rst_n"), bits: 1, idx: None}
}

// Create CFG state machine and returs en/done signals 
fn gen_cfg_state_machine(ir: &SchedCDFGIR, cs: &mut CompilerState, init_actions: &HashMap<Label, Vec<VAssign>>) {
    // make CFG FSM
    let rst_n = make_rst_n();

    let not_reset = VExpr::Var (rst_n.clone());
    let reset = VExpr::UnExp(UnOp::Not, Rc::new(VExpr::Var (rst_n.clone())));

    // State machine init
    {
        let start = ir.start.clone();
        let start_state = get(&cs.states, &start);
        let mut assigns = Vec::<VAssign>::new();
        assigns.push(VAssign {lhs: cs.cur_state.clone(), rhs: start_state.clone()});
        assigns.push(VAssign {lhs: cs.prev_state.clone(), rhs: start_state.clone()});
        for a in init_actions.get(&start).unwrap() {
            assigns.push(a.clone());
        }
        // TODO: Add initialization of ``start'' state
        cs.add_event(reset, assigns);
    }

    for (l, bb) in &ir.cdfg {
        let l_state = get(&cs.states, l);
        let l_done = get(&cs.dones, l);
        let l_en = get(&cs.ens, l);

        let mut conds = vec![not_reset.clone()];
        // cur_satet == l_state
        conds.push(VExpr::BinExp(BinOp::EQ, Rc::new(VExpr::Var (cs.cur_state.clone())), Rc::new(l_state.clone())));
        // l_done
        conds.push(VExpr::Var(l_done.clone()));
        // l_en <= 0;
        let disable = VAssign {lhs: l_en.clone(), rhs: VExpr::Const(0)};
        // prev_state <= cur_state
        let prev_assign = VAssign {lhs: cs.prev_state.clone(), rhs: VExpr::Var (cs.cur_state.clone())};
        cs.add_event(all_true(&conds), vec![disable, prev_assign]);
        
        let mut add_transition = |next_l: &Label, extra_cond: &Option<VExpr>| {
            let next_state = get(&cs.states, next_l).clone();
            let next_en = get(&cs.ens, next_l).clone();
            let enable = VAssign {lhs: next_en, rhs: VExpr::Const(1)};
            let change_cur_state = VAssign {lhs: cs.cur_state.clone(), rhs: next_state.clone()};
            // TODO: add initialization of the next state
            let mut actions = vec![enable, change_cur_state];
            if let Some(inits) = init_actions.get(next_l) {
                for a in inits {
                    actions.push(a.clone());
                }
            }
            match extra_cond {
                None => {
                    cs.add_event(all_true(&conds), actions);
                }
                Some (c) => { 
                    // TODO: Refactor
                    conds.push(c.clone());
                    cs.add_event(all_true(&conds), actions);
                    conds.pop();
                }
            }
        };
        match bb.exit {
            ExitOp::JMP(ref next_l) => {
                add_transition(next_l, &None);
            },
            ExitOp::JC(ref cond, ref next_l1, ref next_l2) => {
                let t_cond = VExpr::Var(ir_var_to_vvar(&cond.clone()));
                let f_cond = VExpr::UnExp(UnOp::Not, Rc::new(t_cond.clone()));
                for (l, cond) in vec![(&next_l1, &t_cond), (&next_l2, &f_cond)] {
                    add_transition(l, &Some(cond.clone()));
                }
            },
            ExitOp::RET => {
                add_transition(&FINISH_STATE.to_string(), &None);
            }
        }
    }
}

fn min_sched_time(dfg: &DFG<Sched>) -> i32 {
    dfg.iter().map(|(_, node)| { node.sched.sched }).min().expect("Error: No nodes.\n")
}

fn max_sched_time(dfg: &DFG<Sched>) -> i32 {
    dfg.iter().map(|(_, node)| { node.sched.sched }).max().expect("Error: No nodes.\n")
}

fn compile_stmt_to_vassign(stmt: &Stmt, prevs: &Vec<Label>, cs: &CompilerState) -> VAssign {
    let vvar = ir_var_to_vvar(&stmt.var);
    let vexpr = ir_expr_to_vexpr(&stmt.expr, None, prevs, cs);
    VAssign { lhs: vvar, rhs: vexpr }
}

// Create seq machine, and returns its initialization actions.
fn gen_seq_machine(l: &Label, dfg: &DFG<Sched>, prevs: &Vec<Label>, cs: &mut CompilerState) -> Vec<VAssign> {
    let en = get(&mut cs.ens, l).clone();
    let done = get(&mut cs.dones, l).clone();

    let cnt = cs.new_reg(&format!("{}_cnt", l), 32, None);
    let mut conds = vec![VExpr::Var(en.clone())];
    let min_time = min_sched_time(dfg);
    let max_time = max_sched_time(dfg);
    debug!("Generating time between {}..{}", min_time, max_time);

    let init_action = VAssign {lhs: cnt.clone(), rhs: VExpr::Const(min_time)};

    let mut sched_stmts = create_stage_stmt_map(dfg);

    for i in min_time..=max_time {
        debug!("Generating time {}", i);
        conds.push(VExpr::BinExp(BinOp::EQ, Rc::new(VExpr::Var(cnt.clone())), Rc::new(VExpr::Const(i))));
        let mut actions = sched_stmts.get(&i).unwrap().iter().map(|stmt|
            compile_stmt_to_vassign(stmt, prevs, cs)
        ).collect::<Vec<_>>();
        // Finalizations
        if i == max_time {
            actions.push(VAssign {lhs: done.clone(), rhs: VExpr::Const(1) });
        }
        cs.add_event(all_true(&conds), actions);
        conds.pop();
    }

    vec![init_action]
}

fn create_stage_stmt_map<'a>(dfg: &'a DFG<Sched>) -> HashMap<i32, Vec<&'a Stmt>> {
    let mut ret = HashMap::<i32, Vec<&'a Stmt>>::new();
    for (_v, node) in dfg {
        ret.entry(node.sched.sched).or_insert(vec![]).push(&node.stmt);
    };
    ret
}

fn is_loop_cond(s: &Stmt) -> bool {
    s.var.name == "loop_cond"
}

fn rename_with_sched(a: &Arg, i: i32, dfg: &HashMap<Var, DFGNode<Sched>>) -> Arg {
    match a {
        Arg::Val(n) => Arg::Val(*n),
        Arg::Var(v) => {
            match dfg.get(v) {
                // Operation chaining
                Some(node) if node.sched.sched == i => Arg::Var(Var { name: format!("{}_wire", v.name) }),
                _ => Arg::Var(v.clone())
            }
        }
    }
}

fn ir_expr_to_vexpr_with_sched(e: &Expr, i: i32, is_first: Option<&VVar>, prevs: &Vec<Label>, cs: &mut CompilerState, dfg: &HashMap<Var, DFGNode<Sched>>) -> VExpr {
    match e {
        Expr::Copy(a) =>
            ir_expr_to_vexpr(&Expr::Copy(rename_with_sched(a, i, dfg)), is_first, prevs, cs),
        Expr::UnExp(op, a) =>
            ir_expr_to_vexpr(&Expr::UnExp(*op, rename_with_sched(a, i, dfg)), is_first, prevs, cs),
        Expr::BinExp(op, a1, a2) =>
            ir_expr_to_vexpr(&Expr::BinExp(*op, rename_with_sched(a1, i, dfg), rename_with_sched(a2, i, dfg)), is_first, prevs, cs),
        Expr::TerExp(op, a1, a2, a3) =>
            ir_expr_to_vexpr(&Expr::TerExp(*op, rename_with_sched(a1, i, dfg), rename_with_sched(a2, i, dfg), rename_with_sched(a3, i, dfg)), is_first, prevs, cs),
    }
}

// Create pipe machine, and returns its initialization actions.
fn gen_pipe_machine(l: &Label, dfg: &DFG<Sched>, prevs: &Vec<Label>, ii: i32, cs: &mut CompilerState) -> Vec<VAssign> {
    let min_stage = min_sched_time(dfg);
    let max_stage = max_sched_time(dfg);
    assert!(min_stage == 0);
    let n_stage = max_stage + 1;
    
    let bits = f64::ceil(f64::log2(f64::from(n_stage))) as i32;
    debug!("bits: {} {} {}", f64::from(n_stage), f64::log2(f64::from(n_stage)), f64::ceil(f64::log2(f64::from(n_stage))));

    let cnt = cs.new_reg(&format!("{}_cnt", l), bits, None);
    let stage_en = cs.new_reg(&format!("{}_stage_en", l), 1, Some(max_stage + 1));
    let stage_is_first = cs.new_reg(&format!("{}_stage_is_first", l), 1, Some(max_stage + 1));
    let is_first = cs.new_reg(&format!("{}_is_first", l), 1, None);
    // let is_pipeline_flush = cs.new_reg(format!("{}_pipeline_flush", l), 1, None);

    let mut inits = vec![];
    inits.push(VAssign {lhs: cnt.clone(), rhs: VExpr::Const(0)});
    for i in 0..=max_stage {
        inits.push(VAssign {lhs: VVar {idx: Some(i), ..stage_en.clone()}, rhs: VExpr::Const(0) });
        inits.push(VAssign {lhs: VVar {idx: Some(i), ..stage_is_first.clone()}, rhs: VExpr::Const(0) });
    }
    inits.push(VAssign {lhs: is_first.clone(), rhs: VExpr::Const(1)});

    let mut conds = vec![VExpr::Var(get(&cs.ens, l).clone())];
    let sched_stmts = create_stage_stmt_map(dfg);

    // stage, cond_wire (no delay), cond_reg (1cycle delay & remains false one cond_wire becomes false)
    let mut loop_conds: Option<(i32, VVar, VVar)> = None;
    // Generate datapath
    for (i, ss) in sched_stmts {
        conds.push(VExpr::Var (VVar {idx: Some(i), ..stage_en.clone()}));
        let mut actions = vec![];
        for s in ss {
            let var = ir_var_to_vvar(&s.var);
            if is_loop_cond(s) {
                let rhs = ir_expr_to_vexpr_with_sched(&s.expr, i, Some(&is_first), prevs, cs, dfg);
                let wire = cs.new_wire(&format!("{}_wire", var.name), var.bits, var.idx, rhs);
                let loop_cond_reg = cs.new_reg(&format!("{}_loop_cond", l), 1, None);
                inits.push(VAssign {lhs: loop_cond_reg.clone(), rhs: VExpr::Const(0)});
                actions.push(VAssign {lhs: loop_cond_reg.clone(), rhs: 
                    VExpr::BinExp(BinOp::And, 
                        Rc::new(VExpr::Var(loop_cond_reg.clone())),
                        Rc::new(VExpr::Var(wire.clone()))
                    )
                });
                loop_conds = Some((i, wire, loop_cond_reg));
            } else {
                let rhs = ir_expr_to_vexpr_with_sched(&s.expr, i, Some(&is_first), prevs, cs, dfg);
                let wire = cs.new_wire(&format!("{}_wire", var.name), var.bits, var.idx, rhs);
                actions.push(VAssign { lhs: var, rhs: VExpr::Var (wire) })
            }
        }
        cs.add_event(all_true(&conds), actions);
        conds.pop();
    }

    let loop_cond = match loop_conds {
        None => VExpr::Const(1),
        Some ((i, loop_cond_wire, loop_cond_reg)) => {
            VExpr::BinExp(BinOp::And,
                Rc::new(VExpr::BinExp(BinOp::Or,
                    Rc::new(VExpr::UnExp(UnOp::Not,
                        Rc::new(VExpr::Var(VVar {idx: Some(i), ..stage_en.clone()})))),
                    Rc::new(VExpr::Var(loop_cond_wire.clone())))),
                Rc::new(VExpr::Var(loop_cond_reg.clone())))
        }
    };

    // Generate pipeline state
    // if (STATE_en)
    {
        let mut actions = vec![];
        // cnt <= cnt == max_time ? 0 cnt + 1;
        actions.push(VAssign {lhs: cnt.clone(), rhs: VExpr::TerExp(
            TerOp::Select,
            Rc::new(VExpr::BinExp(BinOp::EQ, Rc::new(VExpr::Var(cnt.clone())), Rc::new(VExpr::Const(max_stage)))),
            Rc::new(VExpr::Const(0)),
            Rc::new(VExpr::BinExp(BinOp::Plus, Rc::new(VExpr::Var(cnt.clone())), Rc::new(VExpr::Const(1)))),
        )});
        // stage_en[0] <= loop_cond && (cnt == 0)
        actions.push(VAssign {lhs: VVar {idx: Some(0), ..stage_en.clone()}, rhs: VExpr::BinExp(
            BinOp::And,
            Rc::new(loop_cond),
            Rc::new(VExpr::BinExp(BinOp::EQ, Rc::new(VExpr::Var(cnt.clone())), Rc::new(VExpr::Const(0))))
        )});
        for i in 1..=max_stage {
            // stage_en[i] <= stage_en[i - 1];
            actions.push(VAssign {lhs: VVar {idx: Some(i), ..stage_en.clone()}, rhs: VExpr::Var( VVar {idx: Some(i - 1), ..stage_en.clone()})});
        }
        cs.add_event(all_true(&conds), actions);
        
        {
            // if (cnt == 0) {
            conds.push(VExpr::BinExp(BinOp::EQ, Rc::new(VExpr::Var(cnt.clone())), Rc::new(VExpr::Const(0))));
            // if (is_first) {
            conds.push(VExpr::Var(is_first.clone()));
            cs.add_event(all_true(&conds), vec![VAssign {
                lhs: is_first.clone(), rhs: VExpr::Const(0
            )}]);
            // }
            conds.pop();
            cs.add_event(all_true(&conds), vec![VAssign {
                lhs: VVar {idx: Some(0), ..stage_is_first.clone()}, 
                rhs: VExpr::Var(is_first.clone())
            }]);
            // }
            conds.pop();
            for i in 1..=max_stage {
                cs.add_event(all_true(&conds), vec![VAssign {
                    lhs: VVar {idx: Some(i), ..stage_is_first.clone()},
                    rhs: VExpr::Var(VVar {idx: Some(i - 1), ..stage_is_first.clone()})
                }])
            }
            conds.pop();
        }
    }

    inits
}

fn gen_dfg_machine(l: &Label, dfgbb: &DFGBB<Sched, i32>, cs: &mut CompilerState) -> Vec<VAssign> {
    match &dfgbb.body {
        DFGBBBody::Seq(dfg) => {
            gen_seq_machine(l, dfg, &dfgbb.prevs, cs)
        },
        DFGBBBody::Pipe(dfg, ii) => {
            gen_pipe_machine(l, dfg, &dfgbb.prevs, *ii, cs)
        }
    }
}

fn gen_verilog_definitions(ir: &SchedCDFGIR) -> (Vec<(VVar, i32)>, Vec<VVar>, Vec<VAssign>, Vec<VAlways>) {
    let mut cs = CompilerState::init(&ir);
    let mut init_actions = HashMap::<Label, Vec<VAssign>>::new();
    for (l, dfgbb) in &ir.cdfg {
        debug!("Generate label {}", l);
        let actions = gen_dfg_machine(&l, &dfgbb, &mut cs);
        init_actions.insert(String::from(l), actions);
    }
    gen_cfg_state_machine(ir, &mut cs, &init_actions);

    (cs.localparams, cs.regs, cs.wires, cs.always)
}

pub fn compile_sched_cdfg_ir(ir: &SchedCDFGIR) ->VerilogIR {
    let name = &ir.name;
    let io_signals = gen_verilog_io_signals(&ir.params, &ir.returns);
    let (localparams, regs, wires, always) = gen_verilog_definitions(ir);
    VerilogIR { name: name.clone() , localparams, io_signals, regs, wires, always }
}

#[cfg(test)]
mod tests {
    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    use super::*;
    use crate::gen_verilog;

    fn v(s: &str) -> Var {
        var_from_str(s)
    }

    fn a(v: Var) -> Arg {
        Arg::Var(v)
    }

    fn c(n: i32) -> Arg {
        Arg::Val(n)
    }

    fn stmt(v: &str, e: Expr) -> Stmt {
        Stmt {var: Var {name: String::from(v) }, expr: e}
    }

    fn s(v: &str) -> String {
        String::from(v)
    }

    fn e(var: Var, d: DepType) -> Edge {
        Edge {var: var, dep_type: d}
    }
    
    #[test]
    fn collatz_sched_cdfg_ir() {
        init();

        let init = {
            let mut dfg = HashMap::<Var, DFGNode<Sched>>::new();
            dfg.insert(v("cur0"), DFGNode {
                stmt: stmt("cur0", copy(a(v("n")))),
                prevs: vec![],
                succs: vec![e(v("cur1"), DepType::InterBB)],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("step0"), DFGNode {
                stmt: stmt("step0", copy(c(0))),
                prevs: vec![],
                succs: vec![e(v("step1"), DepType::InterBB)],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("test0"), DFGNode {
                stmt: stmt("test0", gt(a(v("n")), c(1))),
                prevs: vec![],
                succs: vec![e(v("test1"), DepType::InterBB)],
                sched: Sched {sched: 0}
            });

            let body = DFGBBBody::Seq(dfg);
            DFGBB {
                prevs: vec![],
                body,
                exit: ExitOp::JC(var_from_str("test0"), label_from_str("LOOP"), label_from_str("EXIT")),
            }
        };

        let loop_ = {
            let mut dfg = HashMap::<Var, DFGNode<Sched>>::new();
            dfg.insert(v("cur1"), DFGNode {
                stmt: stmt("cur1", mu(a(v("cur0")), a(v("cur2")))),
                prevs: vec![e(v("cur0"), DepType::InterBB), e(v("cur2"), DepType::Carried(1)),],
                succs: vec![e(v("tmp1"), DepType::Intra), 
                            e(v("tmp2"), DepType::Intra),
                            e(v("tmp4"), DepType::Intra),
                            e(v("test1"), DepType::Intra),],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("step1"), DFGNode {
                stmt: stmt("step1", mu(a(v("step0")), a(v("step2")))),
                prevs: vec![e(v("step0"), DepType::InterBB), e(v("step2"), DepType::Carried(1)),],
                succs: vec![e(v("step2"), DepType::Intra),],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("tmp1"), DFGNode {
                stmt: stmt("tmp1", div(a(v("cur1")), c(2))),
                prevs: vec![e(v("cur1"), DepType::Intra)],
                succs: vec![e(v("cur2"), DepType::Intra),],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("tmp2"), DFGNode {
                stmt: stmt("tmp2", mult(a(v("cur1")), c(3))),
                prevs: vec![e(v("cur1"), DepType::Intra)],
                succs: vec![e(v("tmp3"), DepType::Intra),],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("tmp3"), DFGNode {
                stmt: stmt("tmp3", plus(a(v("tmp2")), c(1))),
                prevs: vec![e(v("tmp2"), DepType::Intra)],
                succs: vec![e(v("cur2"), DepType::Intra),],
                sched: Sched {sched: 1}
            });
            dfg.insert(v("tmp4"), DFGNode {
                stmt: stmt("tmp4", mod_(a(v("cur1")), c(2))),
                prevs: vec![e(v("cur1"), DepType::Intra)],
                succs: vec![e(v("tmp5"), DepType::Intra),],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("tmp5"), DFGNode {
                stmt: stmt("tmp5", eq(a(v("tmp4")), c(0))),
                prevs: vec![e(v("tmp4"), DepType::Intra)],
                succs: vec![e(v("cur2"), DepType::Intra),],
                sched: Sched {sched: 1}
            });
            dfg.insert(v("cur2"), DFGNode {
                stmt: stmt("cur2", select(a(v("tmp5")), a(v("tmp1")), a(v("tmp3")))),
                prevs: vec![
                    e(v("tmp5"), DepType::Intra), 
                    e(v("tmp1"), DepType::Intra),
                    e(v("tmp3"), DepType::Intra)
                ],
                succs: vec![e(v("cur1"), DepType::Carried(1)),],
                sched: Sched {sched: 1}
            });
            dfg.insert(v("step2"), DFGNode {
                stmt: stmt("step2", plus(a(v("step1")), c(1))),
                prevs: vec![e(v("step1"), DepType::Intra)],
                succs: vec![e(v("step1"), DepType::Carried(1)),],
                sched: Sched {sched: 0}
            });
            dfg.insert(v("loop_cond"), DFGNode {
                stmt: stmt("loop_cond", gt(a(v("cur1")), c(1))),
                prevs: vec![e(v("cur1"), DepType::Intra)],
                succs: vec![],
                sched: Sched {sched: 0}
            });
            let body = DFGBBBody::Pipe(dfg, 2);
            DFGBB {
                prevs: vec![s("INIT")],
                body,
                exit: ExitOp::JMP(s("EXIT")),
            }
        };

        let exit = {
            let mut dfg = HashMap::<Var, DFGNode<Sched>>::new();
            dfg.insert(v("step"), DFGNode {
                stmt: stmt("step", ita(a(v("step0")), a(v("step2")))),
                prevs: vec![e(v("step0"), DepType::InterBB), e(v("step0"), DepType::InterBB),],
                succs: vec![],
                sched: Sched {sched: 0}
            });
            let body = DFGBBBody::Seq(dfg);
            DFGBB {
                prevs: vec![s("INIT"), s("LOOP")],
                body,
                exit: ExitOp::RET,
            }
        };

        let mut cdfg: HashMap<Label, DFGBB<Sched, i32>> = HashMap::new();
        cdfg.insert(String::from("INIT"), init);
        cdfg.insert(String::from("LOOP"), loop_);
        cdfg.insert(String::from("EXIT"), exit);


        let ir = GenCDFGIR {
            name: String::from("collatz"),
            start: String::from("INIT"),
            params: vec![Var {name: String::from("n")}],
            cdfg,
            returns: vec![Var {name: String::from("iter")}],
        };
        let ir = compile_sched_cdfg_ir(&ir);
        gen_verilog::generate_verilog_to_file(&ir, "./tmp/collatz.v");
    }
}