use crate::types::*;

use std::collections::HashMap;
use std::rc::Rc;
use std::iter;

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
    always: Vec<VAlways>
}

impl CompilerState {
    fn new(localparams: Vec<(VVar, i32)>, regs: Vec<VVar>, wires : Vec<VAssign>, always: Vec<VAlways>) -> CompilerState {
        CompilerState {localparams, regs, wires, always}
    }

    fn new_localparam(&mut self, name: &str, v: i32) -> VVar {
        let var = VVar {name: String::from(name), bits: 32, idx: None};
        self.localparams.push((var, v));
        self.localparams.last().unwrap().0.clone()
    }

    fn new_reg(&mut self, name: &str, bits: i32, idx: Option<i32>) -> VVar {
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

fn reduce(es: &Vec<VExpr>, op: BinOp) -> Option<VExpr> {
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

fn ir_var_to_vvar(var: &Var) -> VVar {
    VVar {name: var.name.clone(), bits: 32, idx: None}
}

fn ir_arg_to_vexpr(a: &Arg) -> VExpr {
    match a {
        Arg::Var(v) => VExpr::Var(ir_var_to_vvar(v)),
        Arg::Val(n) => VExpr::Const(*n)
    }
}

fn ir_expr_to_vexpr(e: &Expr) -> VExpr {
    match e {
        Expr::UnExp(op, a) => VExpr::UnExp(*op, Rc::new(ir_arg_to_vexpr(a))),
        Expr::BinExp(op, a1, a2) => {
            match op {
                BinOp::Mu => _,
                BinOp::Ita => _,
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

fn gen_states(ir: &SchedCDFGIR, cs: &mut CompilerState) -> (HashMap<Label, VVar>, HashMap<Label, VVar>, HashMap<Label, VVar>) {
    // Create ens/dones/states;
    let mut ens = HashMap::<Label, VVar>::new();
    let mut dones = HashMap::<Label, VVar>::new();
    let mut states = HashMap::<Label, VVar>::new();

    let finish_state = FINISH_STATE.to_string();
    let labels = ir.cdfg.iter().map(|(l, _)| l).chain(iter::once(&finish_state)).collect::<Vec<_>>();
    {
        let mut cnt = 0;
        for &l in &labels {
            let l_en = cs.new_reg(&format!("{}_en", l), 1, None);
            ens.insert(l.clone(), l_en.clone());
    
            let l_done = cs.new_reg(&format!("{}_done", l), 1, None);
            dones.insert(l.clone(), l_done.clone());

            let var = cs.new_localparam(&format!("{}_ST", l), cnt);
            states.insert(l.clone(), var);
            cnt += 1;
        }
    }
    (ens, dones, states)
}

// Create CFG state machine and returs en/done signals 
fn gen_cfg_state_machine(ir: &SchedCDFGIR, cs: &mut CompilerState, ens: &HashMap<Label, VVar>, dones: &HashMap<Label, VVar>, states: &HashMap<Label, VVar>, init_actions: &HashMap<Label, Vec<VAssign>>) {
    // make CFG FSM
    let cur_state = cs.new_reg("cur_state", 1, None);
    let prev_state = cs.new_reg("prev_state", 1, None);

    let rst_n = make_rst_n();

    let not_reset = VExpr::Var (rst_n.clone());
    let reset = VExpr::UnExp(UnOp::Not, Rc::new(VExpr::Var (rst_n.clone())));

    // State machine init
    {
        let start = ir.start.clone();
        let start_state = get(&states, &start);
        let mut assigns = Vec::<VAssign>::new();
        assigns.push(VAssign {lhs: cur_state.clone(), rhs: VExpr::Var(start_state.clone())});
        assigns.push(VAssign {lhs: prev_state.clone(), rhs: VExpr::Var(start_state.clone())});
        for a in init_actions.get(&start).unwrap() {
            assigns.push(a.clone());
        }
        // TODO: Add initialization of ``start'' state
        cs.add_event(reset, assigns);
    }

    for (l, bb) in &ir.cdfg {
        let l_state = get(&states, l);
        let l_done = get(&dones, l);
        let l_en = get(&ens, l);

        let mut conds = vec![not_reset.clone()];
        // cur_satet == l_state
        conds.push(VExpr::BinExp(BinOp::Eq, Rc::new(VExpr::Var (cur_state.clone())), Rc::new(VExpr::Var(l_state.clone()))));
        // l_done
        conds.push(VExpr::Var(l_done.clone()));
        // l_en <= 0;
        let disable = VAssign {lhs: l_en.clone(), rhs: VExpr::Const(0)};
        // prev_state <= cur_state
        let prev_assign = VAssign {lhs: prev_state.clone(), rhs: VExpr::Var (cur_state.clone())};
        cs.add_event(reduce(&conds, BinOp::And).unwrap(), vec![disable, prev_assign]);
        
        let mut add_transition = |next_l: &Label, extra_cond: &Option<VExpr>| {
            let next_state = get(&states, next_l).clone();
            let next_en = get(&ens, next_l).clone();
            let enable = VAssign {lhs: next_en, rhs: VExpr::Const(1)};
            let change_cur_state = VAssign {lhs: cur_state.clone(), rhs: VExpr::Var(next_state.clone())};
            // TODO: add initialization of the next state
            let mut actions = vec![enable, change_cur_state];
            if let Some(inits) = init_actions.get(next_l) {
                for a in inits {
                    actions.push(a.clone());
                }
            }
            match extra_cond {
                None => {
                    cs.add_event(reduce(&conds, BinOp::And).unwrap(), actions);
                }
                Some (c) => { 
                    // TODO: Refactor
                    conds.push(c.clone());
                    cs.add_event(reduce(&conds, BinOp::And).unwrap(), actions);
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

fn compile_to_action(stmt: &Stmt) -> VAssign {
    let vvar = ir_var_to_vvar(&stmt.var);
    let vexpr = ir_expr_to_vexpr(&stmt.expr);
    VAssign { lhs: vvar, rhs: vexpr }
}

// Create seq machine, and returns its initialization actions.
fn gen_seq_machine(l: &Label, dfg: &DFG<Sched>, en: &VVar, done: &VVar, cs: &mut CompilerState) -> Vec<VAssign> {
    let cnt = cs.new_reg(&format!("{}_cnt", l), 32, None);
    let mut conds = vec![VExpr::Var(en.clone())];
    let min_time = min_sched_time(dfg);
    let max_time = max_sched_time(dfg);
    let init_action = VAssign {lhs: cnt.clone(), rhs: VExpr::Const(min_time)};

    let mut sched_ops = HashMap::<i32, Vec<Stmt>>::new();
    for (_v, node) in dfg {
        sched_ops.entry(node.sched.sched).or_insert(vec![]).push(node.stmt.clone());
    }

    for i in min_time..=max_time {
        conds.push(VExpr::BinExp(BinOp::Eq, Rc::new(VExpr::Var(cnt.clone())), Rc::new(VExpr::Const(i))));
        let mut actions = sched_ops.get(&i).unwrap().iter().map(|stmt|
            compile_to_action(stmt)
        ).collect::<Vec<_>>();
        // Finalizations
        if i == max_time {
            actions.push(VAssign {lhs: done.clone(), rhs: VExpr::Const(1) });
        }
        cs.add_event(reduce(&conds, BinOp::And).unwrap(), actions);
        conds.pop();
    }

    vec![init_action]
}

// Create pipe machine, and returns its initialization actions.
fn gen_pipe_machine(l: &Label, dfg: &DFG<Sched>, cond: &VVar, ii: i32, en: &VVar, done: &VVar, cs: &mut CompilerState) -> Vec<VAssign> {
    // TODO: implement
    vec![]
}

fn gen_dfg_machine(l: &Label, dfgbb: &DFGBB<Sched, i32>, en: &VVar, done: &VVar, cs: &mut CompilerState) -> Vec<VAssign> {
    match &dfgbb.body {
        DFGBBBody::Seq(dfg) => {
            gen_seq_machine(l, dfg, en, done, cs)
        },
        DFGBBBody::Pipe(dfg, cond, ii) => {
            let cond_v = ir_var_to_vvar(cond);
            gen_pipe_machine(l, dfg, &cond_v, *ii, en, done, cs)
        }
    }
}

fn gen_verilog_definitions(ir: &SchedCDFGIR) -> (Vec<(VVar, i32)>, Vec<VVar>, Vec<VAssign>, Vec<VAlways>) {
    let params = Vec::<(VVar, i32)>::new();
    let regs = Vec::<VVar>::new();
    let wires = Vec::<VAssign>::new();
    let always = Vec::<VAlways>::new();

    let mut cs = CompilerState::new(params, regs, wires, always);
    let (ens, dones, states) = gen_states(&ir, &mut cs);
    let mut init_actions = HashMap::<Label, Vec<VAssign>>::new();
    for (l, dfgbb) in &ir.cdfg {
        let actions = gen_dfg_machine(&l, &dfgbb, &ens.get(l).unwrap(), &dones.get(l).unwrap(), &mut cs);
        init_actions.insert(String::from(l), actions);
    }
    gen_cfg_state_machine(ir, &mut cs, &ens, &dones, &states, &init_actions);

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
            dfg.insert(v("cur0"), DFGNode {
                stmt: stmt("cur0", mu(a(v("cur0")), a(v("cur1")))),
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
            dfg.insert(v("test1"), DFGNode {
                stmt: stmt("test1", gt(a(v("cur1")), c(1))),
                prevs: vec![e(v("cur1"), DepType::Intra)],
                succs: vec![],
                sched: Sched {sched: 0}
            });
            let body = DFGBBBody::Pipe(dfg, v("test1"), 2);
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