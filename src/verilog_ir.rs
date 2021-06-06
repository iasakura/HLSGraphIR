use std::fmt;
use std::rc::Rc;

use indexmap::map::IndexMap;

use crate::ir_basic::*;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct VVar {
    pub name: String,
    pub bits: u32,
    pub idx: Option<u32>,
}

impl fmt::Display for VVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        match self.idx {
            None => Ok(()),
            Some(n) => write!(f, "[{}]", n)
        }
    }
}

pub fn vvar<T: fmt::Display>(name: T, bits: u32, idx: Option<u32>) -> VVar {
    VVar {name: name.to_string(), bits, idx}
}

#[derive(Debug, Clone)]
pub enum VExprE {
    UnExp(UnOp, VExpr),
    BinExp(BinOp, VExpr, VExpr),
    TerExp(TerOp, VExpr, VExpr, VExpr),
    Const(Val),
    Var(VVar)
}

#[derive(Debug, Clone)]
pub struct VExpr {
    expr: Rc<VExprE>
}

impl fmt::Display for VExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.expr {
            VExprE::UnExp(op, e) => {
                match op {
                    UnOp::Neg => write!(f, "-({})", e),
                    UnOp::Not => write!(f, "!({})", e),
                }
            },
            VExprE::BinExp(op, e1, e2) => {
                match op {
                    BinOp::Mu => panic!("Mu is not supported in VerilogIR"),
                    _ => write!(f, "({} {} {})", e1, op, e2)
                }
            },
            VExprE::TerExp(op, e1, e2, e3) => {
                match op {
                    TerOp::Select => write!(f, "({} ? {} : {})", e1, e2, e3)
                }
            },
            VExprE::Const(n) => write!(f, "{}", n),
            VExprE::Var(v) => write!(f, "{}", v)
        }
    }
}

pub trait ToVExpr {
    fn to_vexpr(self) -> VExpr;
}

impl ToVExpr for VExpr {
    fn to_vexpr(self) -> VExpr {
        self
    }
}

impl ToVExpr for &VExpr {
    fn to_vexpr(self) -> VExpr {
        VExpr { expr: self.expr.clone() }
    }
}

impl ToVExpr for VVar {
    fn to_vexpr(self) -> VExpr {
        VExpr { expr: Rc::new(VExprE::Var(self)) }
    }
}

impl ToVExpr for &VVar {
    fn to_vexpr(self) -> VExpr {
        VExpr { expr: Rc::new(VExprE::Var(self.clone())) }
    }
}

impl ToVExpr for Val {
    fn to_vexpr(self) -> VExpr {
        VExpr { expr: Rc::new(VExprE::Const(self)) }
    }
}

impl ToVExpr for &Val {
    fn to_vexpr(self) -> VExpr {
        VExpr { expr: Rc::new(VExprE::Const(self.clone())) }
    }
}

pub fn unexp<T1: ToVExpr>(op: UnOp, e1: T1) -> VExpr {
    VExpr { expr: Rc::new(VExprE::UnExp(op, e1.to_vexpr())) }
}

pub fn binexp<T1: ToVExpr, T2: ToVExpr>(op: BinOp, e1: T1, e2: T2) -> VExpr {
    VExpr { expr: Rc::new(VExprE::BinExp(op, e1.to_vexpr(), e2.to_vexpr())) }
}

pub fn terexp<T1: ToVExpr, T2: ToVExpr, T3: ToVExpr>(op: TerOp, e1: T1, e2: T2, e3: T3) -> VExpr {
    VExpr { expr: Rc::new(VExprE::TerExp(op, e1.to_vexpr(), e2.to_vexpr(), e3.to_vexpr())) }
}

macro_rules! gen_vop_def {
    (unop ( $name: ident, $op:expr ) ) => {
        pub fn $name<T: ToVExpr>(e: T) -> VExpr {
            unexp($op, e)
        }
    };

    (binop ( $name: ident, $op:expr ) ) => {
        pub fn $name<T1: ToVExpr, T2: ToVExpr>(e1: T1, e2: T2) -> VExpr {
            binexp($op, e1, e2)
        }
    };

    (terop ( $name: ident, $op:expr ) ) => {
        pub fn $name<T1: ToVExpr, T2: ToVExpr, T3: ToVExpr>(e1: T1, e2: T2, e3: T3) -> VExpr {
            terexp($op, e1, e2, e3)
        }
    };
}

gen_vop_def!{unop( vneg, UnOp::Neg )}
gen_vop_def!{unop( vnot, UnOp::Not )}

gen_vop_def!{binop( vmu, BinOp::Mu )}
gen_vop_def!{binop( vplus, BinOp::Plus )}
gen_vop_def!{binop( vminus, BinOp::Minus )}
gen_vop_def!{binop( vmult, BinOp::Mult )}
gen_vop_def!{binop( vdiv, BinOp::Div )}
gen_vop_def!{binop( vmod_, BinOp::Mod )}
gen_vop_def!{binop( veq, BinOp::EQ )}
gen_vop_def!{binop( vlt, BinOp::LT )}
gen_vop_def!{binop( vle, BinOp::LE )}
gen_vop_def!{binop( vgt, BinOp::GT )}
gen_vop_def!{binop( vge, BinOp::GE )}
gen_vop_def!{binop( vand, BinOp::And )}
gen_vop_def!{binop( vor, BinOp::Or )}

gen_vop_def!{terop( vselect, TerOp::Select )}

#[derive(Debug, Clone)]
pub struct VAssign {
    pub lhs: VVar,
    pub rhs: VExpr
}

pub trait ToVVar {
    fn to_vvar(self) -> VVar;
}

impl ToVVar for VVar {
    fn to_vvar(self) -> VVar {
        self
    }
}

impl ToVVar for &VVar {
    fn to_vvar(self) -> VVar {
        self.clone()
    }
}

pub fn vassign<T1: ToVVar, T2: ToVExpr>(lhs: T1, rhs: T2) -> VAssign {
    VAssign {lhs: lhs.to_vvar(), rhs: rhs.to_vexpr()}
}

#[derive(Debug, Clone, Copy)]
pub enum IOType {
    Input, Output, InputReg, OutputReg,
}

impl fmt::Display for IOType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IOType::Input => write!(f, "input"),
            IOType::InputReg => write!(f, "input logic"),
            IOType::Output => write!(f, "output"),
            IOType::OutputReg => write!(f, "output logic"),
        }
    }
}

#[derive(Debug)]
pub struct VAlways {
    pub clk: VVar,
    pub cond: VExpr,
    pub assigns: Vec<VAssign>,
}

#[derive(Debug)]
pub struct ModuleInstantiation {
    pub name: String,
    pub args: Vec<VVar>
}

// List of case e: var
// Need to be pairwise mutually exclusive
pub type ExMux = Vec<(VExpr, VExpr)>;

#[derive(Debug)]
pub struct VerilogIR {
    pub name: String,
    pub localparams: Vec<(VVar, i32)>,
    pub module_instantiations: Vec<ModuleInstantiation>,
    pub io_signals: Vec<(VVar, IOType)>,
    pub regs: Vec<VVar>,
    pub wires: Vec<VVar>,
    pub assigns: Vec<(VVar, VExpr)>,
    pub always: Vec<VAlways>,
    pub ex_mux: IndexMap<VVar, ExMux>,
}