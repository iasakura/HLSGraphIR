use crate::types::*;
use crate::dfg;

use std::cmp;
use std::collections::HashMap;
use std::rc::Rc;
use std::iter;

use log::{debug, error, log_enabled, info, Level};

const FINISH_STATE : &'static str = "__FINISH";

fn gen_verilog_io_signals(ir_params: &Vec<Var>, ir_returns: &Vec<Var>, cs: &mut CompilerState) {
    let start = "start";
    let finish = "finish";
    let ctrl_sigs = vec![
        ("clk", IOType::Input), 
        ("rst_n", IOType::Input), 
        (start, IOType::Input),
        (finish, IOType::OutputReg)
    ];
    let var_list_of_ctrls = ctrl_sigs.iter().map(|(var, type_)| {
        (vvar(*var, 1, None), *type_)
    });
    let var_list_of_params = ir_params.iter().map(|var| {
        (vvar(&var.name, var.type_.bits, None), IOType::Input)
    });
    let var_list_of_returns = ir_returns.iter().map(|var| {
        (vvar(&var.name, var.type_.bits, None), IOType::OutputReg)
    });
    cs.io = var_list_of_ctrls.chain(var_list_of_params).chain(var_list_of_returns).collect::<Vec<_>>();
    cs.start = vvar(start, 1, None);
    cs.finish = vvar(finish, 1, None);
}

struct CompilerState {
    io: Vec<(VVar, IOType)>,
    localparams: Vec<(VVar, i32)>,
    regs: Vec<VVar>,
    wires : Vec<VAssign>,
    always: Vec<VAlways>,

    cur_state: VVar,
    prev_state: VVar,

    start: VVar,
    finish: VVar,

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

fn bits_of_states(n: i32) -> i32 {
    let res = f64::ceil(f64::log2(f64::from(n))) as i32;
    cmp::max(res, 1)
}

impl CompilerState {
    fn init(ir: &SchedCDFGIR) -> CompilerState {
        // Create initial compiler state. 
        // Don't forget to update cur_state/prev_state!
        let mut cs = CompilerState { 
            io: vec![],
            localparams: vec![], regs: vec![], wires: vec![], always: vec![],
            cur_state: vvar("dummy", 0, None),
            prev_state: vvar("dummy", 0, None),
            start: vvar("dummy", 0, None),
            finish: vvar("dummy", 0, None),
            ens: HashMap::new(), dones: HashMap::new(), states: HashMap::new(),
        };
        gen_verilog_io_signals(&ir.params, &ir.returns, &mut cs);

        // Create states for all CFG block & a final states
        let finish_state = FINISH_STATE.to_string();
        let labels = ir.cdfg.iter().map(|(l, _)| l).chain(iter::once(&finish_state)).collect::<Vec<_>>();

        // TODO: bitwidth
        let nbits = bits_of_states(labels.len() as i32);
        cs.cur_state = cs.new_reg("cur_state", nbits, None);
        cs.prev_state = cs.new_reg("prev_state", nbits, None);

        {
            let mut cnt = 0;
            for &l in &labels {
                let l_en = cs.new_reg(&format!("{}_en", l), 1, None);
                cs.ens.insert(l.clone(), l_en.clone());

                let l_done = cs.new_reg(&format!("{}_done", l), 1, None);
                cs.dones.insert(l.clone(), l_done.clone());

                let var = cs.new_localparam(&format!("{}_ST", l), nbits, cnt);
                cs.states.insert(l.clone(), var.to_vexpr());
                cnt += 1;
            }
        }
        // Create all registers for all SSA variables
        for (_l, dfgbb) in &ir.cdfg {
            let vars = collect_vars(&dfgbb);
            debug!("defined vars of {} = {:?}", _l, vars);
            for v in vars {
                cs.new_reg(&v.name, v.type_.bits, None);
            }
        }

        cs
    }

    fn new_localparam(&mut self, name: &str, bits: i32, v: i32) -> VVar {
        let var = vvar(name, bits, None);
        self.localparams.push((var, v));
        self.localparams.last().unwrap().0.clone()
    }

    fn new_reg(&mut self, name: &str, bits: i32, idx: Option<i32>) -> VVar {
        if log_enabled!(Level::Debug) {
            if let Some(_) = self.regs.iter().find(|x| x.name == name) {
                panic!("{} is already defined.\n", name)
            }
        }
        let v = vvar(name, bits, idx);
        self.regs.push(v);
        self.regs.last().unwrap().clone()
    }

    fn new_wire(&mut self, name: &str, bits: i32, idx: Option<i32>, expr: VExpr) -> VVar {
        let v = vvar(name, bits, idx);
        self.wires.push(vassign(v, &expr));
        self.wires.last().unwrap().lhs.clone()
    }

    fn add_event(&mut self, cond: VExpr, assigns: Vec<VAssign>) {
        let clk = vvar("clk", 1, None);
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
            ret = binexp(op, ret, e);
        }
        Some(ret)
    }
}

fn all_true(es: &Vec<VExpr>) -> VExpr {
    reduce_exprs(es, BinOp::And).unwrap()
}

fn ir_var_to_vvar(var: &Var) -> VVar {
    VVar {name: var.name.clone(), bits: var.type_.bits, idx: None}
}

fn var_to_vvar(v: &Var) -> VVar {
    vvar(&v.name, v.type_.bits, None)
}

fn ir_arg_to_vexpr(a: &Arg) -> VExpr {
    match a {
        Arg::Var(v) => var_to_vvar(v).to_vexpr(),
        Arg::Val(n) => n.to_vexpr(),
    }
}

fn ir_expr_to_vexpr(e: &Expr, is_first: Option<&VVar>, prevs: &Vec<Label>, cs: &CompilerState) -> VExpr {
    match e {
        Expr::UnExp(op, a) => unexp(*op, ir_arg_to_vexpr(a)),
        Expr::BinExp(op, a1, a2) => {
            match op {
                BinOp::Mu => {
                    if let Some(is_first) = is_first {
                        vselect(is_first, ir_arg_to_vexpr(a1), ir_arg_to_vexpr(a2))
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
                    vselect(
                        veq(&cs.prev_state, t_label_value),
                        ir_arg_to_vexpr(a1),
                        ir_arg_to_vexpr(a2)
                    )
                }
                _ => binexp(*op, ir_arg_to_vexpr(a1), ir_arg_to_vexpr(a2)),
            }
        },
        Expr::TerExp(op, a1, a2, a3) => terexp(*op, ir_arg_to_vexpr(a1), ir_arg_to_vexpr(a2), ir_arg_to_vexpr(a3)),
        Expr::Copy(Arg::Val(n)) => n.to_vexpr(),
        Expr::Copy(Arg::Var(v)) => var_to_vvar(v).to_vexpr()
    }
}

fn make_rst_n() -> VVar {
    vvar("rst_n", 1, None)
}

// Create CFG state machine and returs en/done signals 
fn gen_cfg_state_machine(ir: &SchedCDFGIR, cs: &mut CompilerState, init_actions: &HashMap<Label, Vec<VAssign>>) {
    // make CFG FSM
    let rst_n = make_rst_n();

    let not_reset = (&rst_n).to_vexpr();
    let reset = vnot(&rst_n);

    // State machine init
    {
        let start = &ir.start;
        let start_state = get(&cs.states, &start);
        let mut assigns = Vec::<VAssign>::new();
        assigns.push(vassign(&cs.cur_state, start_state));
        assigns.push(vassign(&cs.prev_state, start_state));
        let start_en = cs.ens.get(start).unwrap();
        assigns.push(vassign(start_en, TRUE));
        for a in init_actions.get(start).unwrap() {
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
        conds.push(veq(&cs.cur_state, l_state));
        // l_done
        conds.push(l_done.to_vexpr());
        // l_en <= 0;
        let disable = vassign(l_en, FALSE);
        // prev_state <= cur_state
        let prev_assign = vassign(&cs.prev_state, &cs.cur_state);
        cs.add_event(all_true(&conds), vec![disable, prev_assign]);
        
        let mut add_transition = |next_l: &Label, extra_cond: &Option<VExpr>| {
            let next_state = get(&cs.states, next_l);
            let next_en = get(&cs.ens, next_l);
            let enable = vassign(next_en, TRUE);
            let change_cur_state = vassign(&cs.cur_state, next_state);
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
                let t_cond = var_to_vvar(&cond.clone()).to_vexpr();
                let f_cond = vnot(&t_cond);
                for (l, cond) in vec![(&next_l1, &t_cond), (&next_l2, &f_cond)] {
                    add_transition(l, &Some(cond.clone()));
                }
            },
            ExitOp::RET => {
                add_transition(&FINISH_STATE.to_string(), &None);
            }
        }
    }

    let finish_en = cs.ens.get(&FINISH_STATE.to_string()).unwrap().clone();
    cs.add_event(vand(not_reset.clone(), finish_en), vec![
        vassign(&cs.finish, TRUE)
    ]);
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

    let rst_n = make_rst_n();
    let not_reset = (&rst_n).to_vexpr();

    let min_time = min_sched_time(dfg);
    let max_time = max_sched_time(dfg);
    let n_states = max_time - min_time + 1;
    debug!("Generating time between {}..{}", min_time, max_time);

    let nbits = bits_of_states(n_states);
    let cnt = cs.new_reg(&format!("{}_cnt", l), nbits, None);
    let mut conds = vec![not_reset, en.to_vexpr()];
    let type_ = &Type {bits: nbits, signed: false};

    let init_action = vassign(&cnt, val(min_time, type_.clone()));

    let sched_stmts = create_stage_stmt_map(dfg);

    for i in min_time..=max_time {
        debug!("Generating time {}", i);
        conds.push(veq(&cnt, val(i, type_.clone())));
        let mut actions = sched_stmts.get(&i).unwrap().iter().map(|stmt|
            compile_stmt_to_vassign(stmt, prevs, cs)
        ).collect::<Vec<_>>();
        // Finalizations
        if i == max_time {
            actions.push(vassign(&done, TRUE));
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
        Arg::Val(n) => Arg::Val(n.clone()),
        Arg::Var(v) => {
            match dfg.get(v) {
                // Operation chaining
                Some(node) if node.sched.sched == i => Arg::Var(Var { name: format!("{}_wire", v.name), ..v.clone() }),
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
        // Because second argument of Mu is loop carried dependency, the arg must be read from register.
        Expr::BinExp(BinOp::Mu, a1, a2) =>
            ir_expr_to_vexpr(&Expr::BinExp(BinOp::Mu, rename_with_sched(a1, i, dfg), a2.clone()), is_first, prevs, cs),
        Expr::BinExp(op, a1, a2) =>
            ir_expr_to_vexpr(&Expr::BinExp(*op, rename_with_sched(a1, i, dfg), rename_with_sched(a2, i, dfg)), is_first, prevs, cs),
        Expr::TerExp(op, a1, a2, a3) =>
            ir_expr_to_vexpr(&Expr::TerExp(*op, rename_with_sched(a1, i, dfg), rename_with_sched(a2, i, dfg), rename_with_sched(a3, i, dfg)), is_first, prevs, cs),
    }
}

fn varr_at<T: ToVVar>(arr: T, idx: i32) -> VVar {
    let v = arr.to_vvar();
    vvar(v.name, v.bits, Some(idx))
}

// Create pipe machine, and returns its initialization actions.
fn gen_pipe_machine(l: &Label, dfg: &DFG<Sched>, prevs: &Vec<Label>, ii: i32, cs: &mut CompilerState) -> Vec<VAssign> {
    let min_stage = min_sched_time(dfg);
    let max_stage = max_sched_time(dfg);
    assert!(min_stage == 0);
    let ii_nbits = bits_of_states(ii + 1);

    let cnt = cs.new_reg(&format!("{}_cnt", l), ii_nbits, None);
    let stage_en = cs.new_reg(&format!("{}_stage_en", l), 1, Some(max_stage + 1));
    let stage_is_first = cs.new_reg(&format!("{}_stage_is_first", l), 1, Some(max_stage + 1));
    let is_first = cs.new_reg(&format!("{}_is_first", l), 1, None);
    // let is_pipeline_flush = cs.new_reg(format!("{}_pipeline_flush", l), 1, None);

    let mut inits = vec![];
    inits.push(vassign(cnt.clone(), val(0, uint(ii_nbits))));
    for i in 0..=max_stage {
        inits.push(vassign(varr_at(&stage_en, i), FALSE));
    }
    inits.push(vassign(&is_first, TRUE));
    
    let rst_n = &make_rst_n();

    let mut conds = vec![rst_n.to_vexpr(), get(&cs.ens, l).to_vexpr()];
    let sched_stmts = create_stage_stmt_map(dfg);

    // stage, cond_wire (no delay), cond_reg (1cycle delay & remains false one cond_wire becomes false)
    let mut loop_conds: Option<(i32, VVar, VVar)> = None;
    // Generate datapath
    for (i, ss) in sched_stmts {
        conds.push(varr_at(&stage_en, i).to_vexpr());
        let mut actions = vec![];
        for s in ss {
            let var = ir_var_to_vvar(&s.var);
            if is_loop_cond(s) {
                let rhs = ir_expr_to_vexpr_with_sched(&s.expr, i, Some(&varr_at(&stage_is_first, i)), prevs, cs, dfg);
                let wire = cs.new_wire(&format!("{}_wire", var.name), var.bits, var.idx, rhs);
                let loop_cond_reg = cs.new_reg(&format!("{}_loop_cond", l), 1, None);
                inits.push(vassign(&loop_cond_reg, TRUE));
                actions.push(vassign(&loop_cond_reg, vand(&loop_cond_reg, &wire)));
                loop_conds = Some((i, wire.clone(), loop_cond_reg.clone()));
            } else {
                let rhs = ir_expr_to_vexpr_with_sched(&s.expr, i, Some(&varr_at(&stage_is_first, i)), prevs, cs, dfg);
                let wire = cs.new_wire(&format!("{}_wire", var.name), var.bits, var.idx, rhs);
                actions.push(vassign(var, wire));
            }
        }
        cs.add_event(all_true(&conds), actions);
        conds.pop();
    }

    let loop_cond = match &loop_conds {
        None => TRUE.to_vexpr(),
        Some ((i, loop_cond_wire, loop_cond_reg)) => {
            vand(vor(vnot(varr_at(&stage_en, *i)), loop_cond_wire), loop_cond_reg)
        }
    };

    // Generate pipeline state
    // if (STATE_en)
    {
        let mut actions = vec![];
        // cnt <= cnt == ii ? 0 cnt + 1;
        actions.push(vassign(&cnt, vselect(
            veq(&cnt, val(ii-1, uint(ii_nbits))),
            val(0, uint(ii_nbits)),
            vplus(&cnt, val(1, uint(ii_nbits)))
        )));
        // stage_en[0] <= loop_cond && (cnt == 0)
        actions.push(vassign(varr_at(&stage_en, 0), vand(loop_cond, veq(&cnt, val(0, uint(ii_nbits))))));
        for i in 1..=max_stage {
            // stage_en[i] <= stage_en[i - 1];
            actions.push(vassign(varr_at(&stage_en, i), varr_at(&stage_en, i - 1)));
        }
        cs.add_event(all_true(&conds), actions);
        
        {
            // if (cnt == 0) {
            conds.push(veq(&cnt, val(0, uint(ii_nbits))));
            // if (is_first) {
            conds.push((&is_first).to_vexpr());
            cs.add_event(all_true(&conds), vec![vassign(&is_first, FALSE)]);
            // }
            conds.pop();
            cs.add_event(all_true(&conds), vec![vassign(varr_at(&stage_is_first, 0), &is_first)]);
            // }
            conds.pop();
            for i in 1..=max_stage {
                cs.add_event(all_true(&conds), vec![vassign(
                    varr_at(&stage_is_first, i),
                    varr_at(&stage_is_first, i - 1)
                )])
            }
        }

        if let Some((_, _, loop_cond_reg)) = loop_conds {
            let stage_all_disabled_expr = all_true(&(min_stage..=max_stage).map(|i| 
                vnot(varr_at(&stage_is_first, i))
            ).collect::<Vec<_>>());
            let stage_all_disabled = cs.new_wire(&format!("{}_stage_all_disabled", l), 1, None, stage_all_disabled_expr);
            cs.add_event(all_true(&conds), vec![vassign (
                get(&cs.dones, l),
                vand(vnot(&is_first), vand(vnot(loop_cond_reg), stage_all_disabled))
            )]);
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

fn gen_verilog_definitions(ir: &SchedCDFGIR, cs: &mut CompilerState) {
    let mut init_actions = HashMap::<Label, Vec<VAssign>>::new();
    for (l, dfgbb) in &ir.cdfg {
        debug!("Generate label {}", l);
        let actions = gen_dfg_machine(&l, &dfgbb, cs);
        init_actions.insert(String::from(l), actions);
    }
    gen_cfg_state_machine(ir, cs, &init_actions);
}

pub fn compile_sched_cdfg_ir(ir: &SchedCDFGIR) ->VerilogIR {
    let name = &ir.name;
    let mut cs = CompilerState::init(&ir);
    gen_verilog_definitions(ir, &mut cs);
    VerilogIR { 
        name: name.clone(), 
        localparams: cs.localparams, 
        io_signals: cs.io,
        regs: cs.regs,
        wires: cs.wires,
        always: cs.always 
    }
}

#[cfg(test)]
mod tests {
    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    use super::*;
    use crate::gen_verilog;
    use crate::gen_graphviz;

    fn s(v: &str) -> String {
        String::from(v)
    }
    
    #[test]
    fn collatz_sched_cdfg_ir() {
        init();

        let n = &var("n", int(32));
        let cur0 = &var("cur0", int(32));
        let step0 = &var("step0", int(32));
        let test0 = &var("test0", uint(1));

        let cur1 = &var("cur1", int(32));
        let step1 = &var("step1", int(32));
        let tmp1 = &var("tmp1", int(32));
        let tmp2 = &var("tmp2", int(32));
        let tmp3 = &var("tmp3", int(32));
        let tmp4 = &var("tmp4", int(32));
        let tmp5 = &var("tmp5", int(1));
        let cur2 = &var("cur2", int(32));
        let loop_cond = &var("loop_cond", uint(1));
        let step2 = &var("step2", int(32));

        let step = &var("step", int(32));

        let init = (s("INIT"), DFGBB {
            prevs: vec![],
            body: DFGBBBody::Seq(dfg!{
                cur0 <- copy(n), 0;
                step0 <- copy(val(0, uint(32))), 0;
                test0 <- gt(n, val(1, uint(32))), 0;
            }),
            exit: jc(test0, label("LOOP"), label("EXIT")),
        });

        let loop_ = (s("LOOP"), DFGBB {
            prevs: vec![s("INIT")],
            body: DFGBBBody::Pipe(dfg!{
                cur1 <- mu(cur0, cur2), 0;
                step1 <- mu(step0, step2), 0;
                tmp1 <- div(cur1, val(2, int(32))), 0;
                tmp2 <- mult(cur1, val(3, int(32))), 0;
                tmp3 <- plus(tmp2, val(1, int(32))), 1;
                tmp4 <- mod_(cur1, val(2, int(32))), 0;
                tmp5 <- eq(tmp4, val(0, int(32))), 1;
                cur2 <- select(tmp5, tmp1, tmp3), 1;
                step2 <- plus(step1, val(1, int(32))), 1;
                loop_cond <- gt(cur2, val(1, int(32))), 1;
            }, 2),            
            exit: ExitOp::JMP(s("EXIT")),
        });

        let exit = (s("EXIT"), DFGBB {
            prevs: vec![s("INIT"), s("LOOP")],
            body: DFGBBBody::Seq(dfg!{
                step <- ita(step0, step2), 0;
            }),
            exit: ExitOp::RET,
        });

        let cdfg = vec![init, loop_, exit].into_iter().collect::<HashMap<_, _>>();


        let ir = GenCDFGIR {
            name: String::from("collatz"),
            start: String::from("INIT"),
            params: vec![Var {name: String::from("n"), type_: int(32)}],
            cdfg,
            returns: vec![Var {name: String::from("step"), type_: int(32)}],
        };
        let verilog = compile_sched_cdfg_ir(&ir);
        gen_verilog::generate_verilog_to_file(&verilog, "./test/collatz.v");
        
        if let DFGBBBody::Pipe(ref dfg, _) = ir.cdfg.get("LOOP").unwrap().body {
            gen_graphviz::gen_graphviz_from_dfg(dfg, "./test/collatz.dot")
        }
    }

    #[test]
    fn collatz_ii_1_sched_cdfg_ir() {
        let n = &var("n", int(32));

        let init_cur0 = &var("init_cur0", int(32));
        let init_step0 = &var("init_step0", int(32));
        let init_test0 = &var("init_test0", int(1));

        let init_even = &var("init_even", int(32));
        let init_odd_tmp = &var("init_odd_tmp", int(32));
        let init_odd = &var("init_odd", int(32));
        let init_mod = &var("init_mod", int(32));
        let init_eq1 = &var("init_eq1", int(1));
        let init_cur1 = &var("init_cur1", int(32));

        let cur0 = &var("cur0", int(32));
        let cur1 = &var("cur1", int(32));
        let step = &var("step", int(32));
        let loop_step = &var("loop_step", int(32));

        // loop_tmp = select(x % 4 == 0, x / 4, select(x % 2 == 0, 3 (x / 2) + 1, (3 * x + 1) / 2))
        let loop_if_e1 = &var("loop_if_e1", int(32));
        let loop_if_e2_tmp1 = &var("loop_if_e2_tmp1", int(32));
        let loop_if_e2_tmp2 = &var("loop_if_e2_tmp2", int(32));
        let loop_if_e2 = &var("loop_if_e2", int(32));
        let loop_if_e3_tmp1 = &var("loop_if_e3_tmp1", int(32));
        let loop_if_e3_tmp2 = &var("loop_if_e3_tmp2", int(32));
        let loop_if_e3 = &var("loop_if_e3", int(32));
        let loop_if_else_cond_tmp = &var("loop_if_else_cond_tmp", int(32));
        let loop_if_else_cond = &var("loop_if_else_cond", int(1));
        let loop_if_else = &var("loop_if_else", int(32));
        let loop_if_cond_tmp = &var("loop_if_cond_tmp", int(32));
        let loop_if_cond = &var("loop_if_cond", int(1));
        let loop_if = &var("loop_if", int(32));

        let loop_cur0 = &var("loop_cur0", int(32));
        let loop_cur1 = &var("loop_cur1", int(32));
        let loop_cond = &var("loop_cond", int(1));

        let res = &var("res", int(32));

        let cdfg = vec![
            (s("INIT0"), DFGBB {
                prevs: vec![],
                body: DFGBBBody::Seq(dfg!{
                    init_cur0 <- copy(n), 0;
                    init_step0 <- copy(val(0, int(32))), 0;
                    init_test0 <- gt(n, val(1, int(32))), 0;
                }),
                exit: jc(init_test0, label("INIT1"), label("EXIT"))
            }),
            (s("INIT1"), DFGBB {
                prevs: vec![label("INIT0")],
                body: DFGBBBody::Seq(dfg!{
                    init_even <- div(init_cur0, val(2, int(32))), 0;
                    init_odd_tmp <- mult(init_cur0, val(3, int(32))), 1;
                    init_odd <- plus(init_odd_tmp, val(1, int(32))), 2;
                    init_mod <- mod_(init_cur0, val(2, int(32))), 0;
                    init_eq1 <- eq(init_mod, val(0, int(32))), 0;
                    init_cur1 <- select(init_eq1, init_even, init_odd), 2;
                }),
                exit: jmp(label("LOOP"))
            }),
            (s("LOOP"), DFGBB {
                prevs: vec![label("INIT1")],
                body: DFGBBBody::Pipe(dfg!{
                    cur0 <- mu(init_cur0, loop_cur0), 0;
                    cur1 <- mu(init_cur1, loop_cur1), 1;
                    step <- mu(init_step0, loop_step), 0;
                    loop_step <- plus(step, val(1, int(32))), 1;

                    // loop_tmp <- select(x % 4 == 0, x / 4, select(x % 2 == 0, 3 (x / 2) + 1, (3 * x + 1) / 2))
                    loop_if_e1 <- div(cur0, val(4, int(32))), 1;
                    loop_if_e2_tmp1 <- div(cur0, val(2, int(32))), 0;
                    loop_if_e2_tmp2 <- mult(val(3, int(32)), loop_if_e2_tmp1), 1;
                    loop_if_e2 <- plus(loop_if_e2_tmp2, val(1, int(32))), 2;
                    loop_if_e3_tmp1 <- mult(val(3, int(32)), cur0), 1;
                    loop_if_e3_tmp2 <- plus(loop_if_e3_tmp1, val(1, int(32))), 2;
                    loop_if_e3 <- div(loop_if_e3_tmp2, val(2, int(32))), 2;
                    loop_if_else_cond_tmp <- mod_(cur0, val(2, int(32))), 0;
                    loop_if_else_cond <- eq(loop_if_else_cond_tmp, val(0, int(32))), 1;
                    loop_if_else <- select(loop_if_else_cond, loop_if_e2, loop_if_e3), 2;
                    loop_if_cond_tmp <- mod_(cur0, val(4, int(32))), 0;
                    loop_if_cond <- eq(loop_if_cond_tmp, val(0, int(32))), 1;
                    loop_if <- select(loop_if_cond, loop_if_e1, loop_if_else), 2;

                    loop_cur0 <- copy(cur1), 1;
                    loop_cur1 <- copy(loop_if), 2;
                    loop_cond <- gt(cur0, val(1, int(32))), 0;
                }, 1),
                exit: jmp(label("EXIT"))
            }),
            (s("EXIT"), DFGBB {
                prevs: vec![label("INIT0"), label("LOOP")],
                body: DFGBBBody::Seq(dfg!{
                    res <- ita(init_step0, step), 0;
                }),
                exit: ret()
            })
        ].into_iter().collect::<HashMap<_, _>>();

        let ir = GenCDFGIR {
            name: "collatz_ii1".to_string(),
            start: label("INIT0"),
            params: vec![n.clone()],
            cdfg,
            returns: vec![res.clone()]
        };

        let verilog = compile_sched_cdfg_ir(&ir);
        gen_verilog::generate_verilog_to_file(&verilog, "./test/collatz_ii1/collatz_ii1.v");

        for (l, dfg) in ir.cdfg {
            match &dfg.body {
                DFGBBBody::Seq(dfg) | DFGBBBody::Pipe(dfg, _) => 
                    gen_graphviz::gen_graphviz_from_dfg(dfg, &format!("./test/collatz_ii1/{}.dot", l))
            }
        }
    }
}