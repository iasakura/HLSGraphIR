use crate::types::*;

use std::collections::HashMap;

fn gen_verilog_io_params(params: Vec<Var>, returns: Vec<Var>) -> Vec<(VVar, IOType)> {
    let var_list_of_params = params.iter().map(|var| {
        (VVar {name: var.name.clone(), bits: 32, idx: None}, IOType::Input)
    });
    let var_list_of_returns = returns.iter().map(|var| {
        (VVar {name: var.name.clone(), bits: 32, idx: None}, IOType::OutputReg)
    });
    var_list_of_params.chain(var_list_of_returns).collect::<Vec<_>>()
}

struct CompilerState {
    regs: Vec<VVar>,
    wires : Vec<VAssign>,
    always: Vec<VAlways>
}

impl CompilerState {
    fn new(regs: Vec<VVar>, wires : Vec<VAssign>, always: Vec<VAlways>) -> CompilerState {
        CompilerState {regs, wires, always}
    }

    fn new_reg(&mut self, name: &str, bits: i32, idx: Option<i32>) -> VVar {
        let v = VVar {name: String::from(name), bits, idx};
        let res = v.clone();
        self.regs.push(v);
        res
    }

    fn new_wire(&mut self, name: &str, bits: i32, idx: Option<i32>, expr: VExpr) -> VVar {
        let v = VVar {name: String::from(name), bits, idx};
        let res = v.clone();
        self.wires.push(VAssign { lhs: v, rhs: expr.clone() });
        res
    }

    fn add_event(&mut self, cond: VExpr, assigns: Vec<VAssign>) {
        let clk = VVar {name: String::from("clk"), bits: 1, idx: None};
        self.always.push(VAlways {clk, cond, assigns})
    }
}

fn get<'a, K : std::hash::Hash + std::cmp::Eq + std::fmt::Debug, V>(map: &'a HashMap<K, V>, key: &K) -> &'a V {
    map.get(key).expect(format!("{:?} is not found.\n", key))
}

fn reduce(es: &Vec<VExpr>, op: BinOp) -> Option<VExpr> {
    if es.len() == 0 {
        None
    } else {
        let mut ret = es[0];
        for e in &es[1..] {
            ret = VExpr::BinExp(op, Box::new(ret), Box::new(e.clone()));
        }
        Some(ret)
    }
}

fn gen_verilog_definitions(ir: SchedCDFGIR) -> (Vec<VVar>, Vec<VAssign>, Vec<VAlways>) {
    let regs = Vec::<VVar>::new();
    let wires = Vec::<VAssign>::new();
    let always = Vec::<VAlways>::new();

    let mut cs = CompilerState::new(regs, wires, always);

    // make CFG FSM
    let cur_state = cs.new_reg("cur_state", 1, None);
    let prev_state = cs.new_reg("prev_state", 1, None);

    let ens = HashMap::<Label, VVar>::new();
    let dones = HashMap::<Label, VVar>::new();
    let states = HashMap::<Label, i32>::new();

    let rst_n = VVar {name: "rst_n".to_string(), bits: 1, idx: None};

    let not_reset = VExpr::Var (rst_n);
    let reset = VExpr::UnExp(UnOp::Not, Box::new(VExpr::Var (rst_n)));

    {
        let mut cnt = 0;
        for (l, dfg) in ir.cdfg {
            let l_en = cs.new_reg(format!("{}_en", l), 1, None);
            ens.insert(l, l_en);
    
            let l_done = cs.new_reg(format!("{}_en", l), 1, None);
            dones.insert(l, l_done);

            states.insert(l, cnt);
            cnt += 1;
        }
    }
    
    
    // State machine init
    {
        let start = ir.start;
        let start_state = get(&states, &start);
        let assigns = Vec::<VAssign>::new();
        assigns.push(VAssign {lhs: cur_state, rhs: VExpr::Const(*start_state)});
        assigns.push(VAssign {lhs: prev_state, rhs: VExpr::Const(*start_state)});
        cs.add_event(reset, assigns);
    }
    for (l, bb) in &ir.cdfg {
        let &l_state = get(&states, l);
        let &l_done = get(&dones, l);
        let &l_en = get(&ens, l);

        let conds = vec![not_reset];
        // cur_satet == l_state
        conds.push(VExpr::BinExp(BinOp::Eq, Box::new(VExpr::Var (cur_state)), Box::new(VExpr::Const(l_state))));
        // l_done
        conds.push(VExpr::Var(l_done));
        // l_en <= 0;
        let disable = VAssign {lhs: l_en, rhs: VExpr::Const(0)};
        // prev_state <= cur_state
        let prev_assign = VAssign {lhs: prev_state, rhs: VExpr::Var (cur_state)};
        cs.add_event(reduce(&conds, BinOp::And).unwrap(), vec![disable, prev_assign]);
        
        match bb.exit {
            ExitOp::JMP(ref next_l) => {
                let &next_state = get(&states, next_l);
                let &next_en = get(&ens, next_l);
                let enable = VAssign {lhs: next_en, rhs: VExpr::Const(1)};
                let change_cur_state = VAssign {lhs: cur_state, rhs: VExpr::Const(next_state)};
                cs.add_event(reduce(&conds, BinOp::And).unwrap(), vec![enable, change_cur_state]);
            },
            ExitOp::JC(cond, next_l1, next_l2) => {
                // TODO
            },
            ExitOp::RET => {
                // TODO
            }
        }
    }

    (cs.regs, cs.wires, cs.always)
}

pub fn compile_sched_cdfg_ir(ir: SchedCDFGIR) ->VerilogIR {
    let name = ir.name;
    let io_params = gen_verilog_io_params(ir.params, ir.returns);
    let (regs, wires, always) = gen_verilog_definitions(ir);
    VerilogIR { name, io_params, regs, wires, always }
}

