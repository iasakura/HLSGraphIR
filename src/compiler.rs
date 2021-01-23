use types::*;

fn gen_verilog_definitions(params: Vec<Var>, returns: Vec<Var>) -> Vec<(VVar, IOType)> {
    let var_list_of_params = params.iter().map(|var| {
        (VVar {name: var.clone(), bits: 32})
    });
    let var_list_of_returns = returns.iter().map(|var| {
        (VVar {name: var.clone(), bits: 32})
    });
    var_list_of_params.extend(var_list_of_returns).collect::<Vec<_>>()
}

pub fn compile_sched_cdfg_ir(ir: SchedCDFGIR) ->VerilogIR {
    let name = ir.name;
    let io_params = gen_verilog_io_params(ir.params, ir.returns);
    let (regs, wires, always) = gen_verilog_definitions(ir);
    VerilogIR { name: io_params, regs, wires, always }
}

