use std::fs;
use std::io;
use std::rc::Rc;
use std::cell::RefCell;

use indoc::indoc;

use crate::types::*;

fn make_var_decl(var_spec: &str, name: &str, bits: i32, arr_size: Option<i32>) -> String {
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
    cur_tab: Rc<RefCell<i32>>
}

impl Drop for Scope {
    fn drop(&mut self) {
        let t = *self.cur_tab.borrow() - 4;
        *self.cur_tab.borrow_mut() = t;
    }
}

impl Scope {
    fn new(cur_tab: Rc<RefCell<i32>>) -> Scope {
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
    };
    

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
            genstmt!("{}", make_var_decl("reg", &v.name, v.bits, v.idx));
        }
        for w in &ir.wires {
            genstmt!("{}", make_var_decl("wire", &w.lhs.name, w.lhs.bits, None));
        }
        for w in &ir.wires {
            genstmt!("assign {} = {}", w.lhs, w.rhs);
        }
        for a in &ir.always {
            genln!("always @(posedge {}) begin", a.clk.name);
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
    }
    genln!("endmodule");
}

pub fn generate_verilog_to_file(ir: &VerilogIR, filename: &str) {
    let mut stream = io::BufWriter::new(fs::File::create(filename).unwrap());
    generate_verilog_to_stream(ir, &mut stream);
}

#[test]
fn generate_verilog_test() {
    let mut vec: Vec<u8> = Vec::new();
    let ir = VerilogIR {
        name: String::from("test_module"),
        localparams: vec![
            (VVar {name: String::from("CONSTANT"), bits: 1, idx: None }, 42),
        ],
        io_signals: vec![
            (VVar {name: String::from("clk"), bits: 1, idx: None}, IOType::Input), 
            (VVar {name: String::from("rst_n"), bits: 1, idx: None}, IOType::Input),
            (VVar {name: String::from("start"), bits: 1, idx: None}, IOType::Input),
            (VVar {name: String::from("finish"), bits: 1, idx: None}, IOType::OutputReg),
            (VVar {name: String::from("n"), bits: 32, idx: None}, IOType::Input),
            (VVar {name: String::from("ret"), bits: 32, idx: None}, IOType::OutputReg)
        ],
        regs: vec![VVar {name: String::from("a"), bits: 32, idx: Some(3)}, VVar {name: String::from("b"), bits: 64, idx: None}],
        wires: vec![VAssign {lhs: VVar {name: String::from("c"), bits: 32, idx: None}, rhs: VExpr::BinExp(BinOp::Plus, Rc::new(VExpr::Var(VVar {name: String::from("a"), bits: 32, idx: Some(0)})), Rc::new(VExpr::Const(val(1, int(32)))) )}],
        always: vec![
            VAlways {
                clk: VVar {name: String::from("clk"), bits: 1, idx: None},
                cond: VExpr::BinExp(BinOp::LT, 
                    Rc::new(VExpr::Var(VVar {name: String::from("a"), bits: 32, idx: Some(0)})), 
                    Rc::new(VExpr::Var(VVar {name: String::from("b"), bits: 32, idx: None}))), 
                assigns: vec![
                    VAssign {lhs: VVar {name: String::from("a"), bits: 32, idx: Some(0)}, rhs: VExpr::Var(VVar {name: String::from("c"), bits: 32, idx: None})},
                ]
            },
        ]
    };
    generate_verilog_to_stream(&ir, &mut vec);
    let s = vec.iter().map(|&u| u as char).collect::<String>();
    println!("verilog: \n {}", s);
}
