use std::fs;
use std::io;
use std::rc::Rc;
use std::cell::RefCell;

use indoc::indoc;

use crate::verilog_ir::*;

fn make_var_decl(var_spec: &str, name: &str, bits: u32, arr_size: Option<u32>) -> String {
    let mut ret = var_spec.to_string();
    ret.push_str(" ");
    if bits != 1 {
        ret.push_str(&format!("[{}:0] ", bits - 1));
    }
    ret.push_str(name);
    match arr_size {
        None => (),
        Some(arr_size) => ret.push_str(&format!("[0:{}]", arr_size - 1))
    }
    ret
}

struct Scope {
    cur_tab: Rc<RefCell<u32>>
}

impl Drop for Scope {
    fn drop(&mut self) {
        let t = *self.cur_tab.borrow() - 4;
        *self.cur_tab.borrow_mut() = t;
    }
}

impl Scope {
    fn new(cur_tab: Rc<RefCell<u32>>) -> Scope {
        *cur_tab.borrow_mut() += 4;
        Scope {cur_tab}
    }
}

pub fn generate_verilog_to_stream(ir: &VerilogIR, stream: &mut impl io::Write) {
    let cur_tab = Rc::new(RefCell::new(0));
    stream.write(indoc!{r#"
        `define to_int(val) \
            {{31{1'b0}}, (val)}
        `define to_bool(val) \
            (val != 0)

    "#}.as_bytes()).unwrap();

    macro_rules! gen {
        ( $( $e:expr ),* ) => {
            for _ in 0..*cur_tab.borrow() {
                stream.write(" ".as_bytes()).unwrap();
            }
            stream.write(format!( $( $e ),* ).as_bytes()).unwrap();
        };
    }
    macro_rules! genln {
        ( $( $e:expr ),* ) => {
            gen!($($e),*);
            stream.write("\n".as_bytes()).unwrap();
        };
    }
    macro_rules! genstmt {
        ( $( $e:expr ),* ) => {
            gen!( $($e),* );
            stream.write(";\n".as_bytes()).unwrap();
        }
    }

    genln!("module {}(", ir.name);
    {
        let _s = Scope::new(cur_tab.clone());
        let args = ir.io_signals.iter().map(|(v, _)| v.name.clone()).collect::<Vec<_>>().join(",\n    ");
        genln!("{}", &args);
    }
    gen!(");\n");
    {
        let _s = Scope::new(cur_tab.clone());
        for (v, io) in &ir.io_signals {
            genstmt!("{}", make_var_decl(&io.to_string(), &v.name, v.bits, None));
        }
        for p in &ir.localparams {
            genstmt!("localparam {} = {}", p.0, p.1);
        }
        for v in &ir.regs {
            genstmt!("{}", make_var_decl("logic", &v.name, v.bits, v.idx));
        }
        for var in &ir.wires {
            genstmt!("{}", make_var_decl("logic", &var.name, var.bits, None));
        }
        for m in &ir.module_instantiations {
            let args_str = m.args.iter().map(|a| a.name.clone()).collect::<Vec<_>>().join(", ");
            genstmt!("{}({});", m.name, args_str);
        }
        for (var, rhs) in &ir.assigns {
            genstmt!("assign {} = {}", var, rhs);
        }
        for a in &ir.always {
            genln!("always_ff @(posedge {}) begin", a.clk.name);
            {
                let _s = Scope::new(cur_tab.clone());
                genln!("if ({}) begin", a.cond);
                {
                    let _s = Scope::new(cur_tab.clone());
                    for assign in &a.assigns {
                        genstmt!("{} <= {}", assign.lhs, assign.rhs);
                    }
                }
                genln!("end");
            }
            genln!("end");
        }
        for (lhs, mux) in &ir.ex_mux {
            // genstmt!("{}", make_var_decl("wire", &lhs.name, lhs.bits, None));
            genln!("always_comb begin");
            {
                let _s = Scope::new(cur_tab.clone());
                let case_list = mux.iter().map(|(c, _)| c.to_string()).collect::<Vec<_>>().join(", ");
                let n = mux.len();
                genln!("unique casez ({{ {} }})", case_list);
                {
                    let _s = Scope::new(cur_tab.clone());
                    for (i, (_, rhs)) in mux.iter().enumerate() {
                        let case_cond = (0..n).map(|j|
                            if i == j { "1" }
                            else { "0" }
                        ).collect::<Vec<_>>().join("");
                        genstmt!("{}'b{}: {} = {}", n, case_cond, lhs.name, rhs);
                    }
                    let lhs_bits = lhs.bits;
                    let undef = format!("{}'b{}", lhs_bits, "X".repeat(lhs_bits as usize));
                    genstmt!("default: {} = {}", lhs.name, undef);
                }
                genln!("endcase");
            }
            genln!("end");
        }
    }
    genln!("endmodule");
}

pub fn generate_verilog_to_file(ir: &VerilogIR, filename: &str) {
    let mut stream = io::BufWriter::new(fs::File::create(filename).unwrap());
    generate_verilog_to_stream(ir, &mut stream);
}

#[test]
fn generate_verilog_test() {
    use crate::ir_basic::*;

    use indexmap::map::IndexMap;

    let mut vec: Vec<u8> = Vec::new();
    let ir = VerilogIR {
        name: String::from("test_module"),
        localparams: vec![
            (vvar("CONSTANT", 1, None), 42),
        ],
        io_signals: vec![
            (vvar("clk", 1, None), IOType::Input),
            (vvar("rst_n", 1, None), IOType::Input),
            (vvar("start", 1, None), IOType::Input),
            (vvar("finish", 1, None), IOType::OutputReg),
            (vvar("n", 32, None), IOType::Input),
            (vvar("ret", 32, None), IOType::OutputReg)
        ],
        module_instantiations: vec![], // TODO: Add sample case
        regs: vec![
            vvar("a", 32, Some(3)),
            vvar("b", 64, None)
        ],
        wires: vec![
            vvar("c", 32, None),
            vvar("d", 32, None),
            vvar("e", 32, None),
        ],
        assigns: vec![
            (vvar("c", 32, None), vplus(vvar("a", 32, Some(0)), val(1, int(32)))),
            (vvar("d", 32, None), val(42, int(32)).to_vexpr()),
            (vvar("e", 32, None), val(-1, int(32)).to_vexpr()),
        ],
        always: vec![
            VAlways {
                clk: vvar("clk", 1, None),
                cond: vlt(vvar("a", 32, Some(0)), vvar("b", 32, None)),
                assigns: vec![
                    vassign(vvar("a", 32, Some(0)), vvar("c", 32, None).to_vexpr()),
                ]
            },
        ],
        ex_mux: vec![
            (vvar("ex_mux_test1", 32, None), vec![
                (vlt(vvar("a", 32, Some(0)), vvar("b", 32, None)), vvar("d", 32, None).to_vexpr()),
                (vge(vvar("a", 32, Some(0)), vvar("b", 32, None)), vvar("e", 32, None).to_vexpr()),
            ]),
        ].into_iter().collect::<IndexMap<_, _>>(),
    };
    generate_verilog_to_stream(&ir, &mut vec);
    let s = vec.iter().map(|&u| u as char).collect::<String>();
    println!("verilog: \n {}", s);
}
