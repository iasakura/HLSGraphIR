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
        ret.push_str(&format!("[{},0] ", bits - 1));
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
    let mut cur_tab = Rc::new(RefCell::new(0));
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
            stream.write("\n".as_bytes());
        };
    }
    

    genln!("module {}(", ir.name);
    {
        let _s = Scope::new(cur_tab.clone());
        for (v, _) in &ir.io_params {
            genln!("{},", v.name);
        }
    }
    gen!(");\n");
    {
        let _s = Scope::new(cur_tab.clone());
        for (v, io) in &ir.io_params {
            genln!("{}", make_var_decl(&io.to_string(), &v.name, v.bits, None));
        }
        for v in &ir.regs {
            genln!("{}", make_var_decl("reg", &v.name, v.bits, v.idx));
        }
        for w in &ir.wires {
            genln!("{}", make_var_decl("wire", &w.lhs.name, w.lhs.bits, None));
        }
        for w in &ir.wires {
            genln!("assign {} = {}", w.lhs, w.rhs);
        }
        for (clk, cond, assigns) in &ir.always {
            genln!("always @(posedge {}) begin", clk.name);
            {
                let _s = Scope::new(cur_tab.clone());
                genln!("if ({}) begin", cond);
                {
                    let _s = Scope::new(cur_tab.clone());
                    for assign in assigns {
                        genln!("{} <= {}", assign.lhs, assign.rhs);
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
    ()
}

#[test]
fn generate_verilog_test() {
    let mut vec: Vec<u8> = Vec::new();
    let ir = VerilogIR {
        name: String::from("test_module"),
        io_params: vec![
            (VVar {name: String::from("clk"), bits: 1, idx: None}, IOType::Input), 
            (VVar {name: String::from("rst_n"), bits: 1, idx: None}, IOType::Input),
            (VVar {name: String::from("start"), bits: 1, idx: None}, IOType::Input),
            (VVar {name: String::from("finish"), bits: 1, idx: None}, IOType::OutputReg),
            (VVar {name: String::from("n"), bits: 32, idx: None}, IOType::Input),
            (VVar {name: String::from("ret"), bits: 32, idx: None}, IOType::OutputReg)
        ],
        regs: vec![VVar {name: String::from("a"), bits: 32, idx: Some(3)}, VVar {name: String::from("b"), bits: 64, idx: None}],
        wires: vec![VAssign {lhs: VVar {name: String::from("c"), bits: 32, idx: None}, rhs: VExpr::BinExp(BinOp::Plus, Box::new(VExpr::Var(VVar {name: String::from("a"), bits: 32, idx: Some(0)})), Box::new(VExpr::Const(1)) )}],
        always: vec![
            (VVar {name: String::from("clk"), bits: 1, idx: None},
             VExpr::BinExp(BinOp::LT, 
                Box::new(VExpr::Var(VVar {name: String::from("a"), bits: 32, idx: Some(0)})), 
                Box::new(VExpr::Var(VVar {name: String::from("b"), bits: 32, idx: None}))), 
                vec![
                    VAssign {lhs: VVar {name: String::from("a"), bits: 32, idx: Some(0)}, rhs: VExpr::Var(VVar {name: String::from("c"), bits: 32, idx: None})},
                ]
            ),
        ]
    };
    generate_verilog_to_stream(&ir, &mut vec);
    let str = vec.iter().map(|&s| s as char).collect::<String>();
    println!("verilog: \n {}", str);
}
