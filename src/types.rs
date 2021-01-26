use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Var {
    pub name: String,
}

pub fn var_from_str(s: &str) -> Var {
    Var { name : String::from(s) }
}

pub fn var_from_string(s: String) -> Var {
    Var { name : s }
}

pub type Label = String;
pub fn label_from_str(s: &str) -> Label {
    String::from(s)
}

#[derive(Debug)]
pub enum Arg {
    Var(Var),
    Val(i32)
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "-")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,

    Eq,
    LT,
    GT,
    LE,
    GE,

    And,
    Or,

    Mu,
    Ita
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Mult => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),

            BinOp::Eq => write!(f, "=="),
            BinOp::LT => write!(f, "<"),
            BinOp::GT => write!(f, ">"),
            BinOp::LE => write!(f, "<="),
            BinOp::GE => write!(f, ">="),

            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),

            BinOp::Mu => write!(f, "MU"),
            BinOp::Ita => write!(f, "Ita")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TerOp {
    Select
}

#[derive(Debug)]
pub enum Expr {
    Copy (Arg),
    UnExp (UnOp, Arg),
    BinExp (BinOp, Arg, Arg),
    TerExp (TerOp, Arg, Arg, Arg)
}

pub fn copy(a : Arg) -> Expr {
    Expr::Copy (a)
}

pub fn neg(a : Arg) -> Expr {
    Expr::UnExp(UnOp::Neg, a)
}

pub fn mu(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Mu, a1, a2)
}

pub fn ita(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Ita, a1, a2)
}

pub fn plus(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Plus, a1, a2)
}

pub fn minus(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Minus, a1, a2)
}

pub fn mult(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Mult, a1, a2)
}

pub fn div(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Div, a1, a2)
}

pub fn mod_(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Mod, a1, a2)
}

pub fn eq(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::Eq, a1, a2)
}

pub fn lt(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::LT, a1, a2)
}

pub fn le(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::LE, a1, a2)
}

pub fn gt(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::GT, a1, a2)
}

pub fn ge(a1 : Arg, a2 : Arg) -> Expr {
    Expr::BinExp(BinOp::GE, a1, a2)
}

pub fn select(a1 : Arg, a2 : Arg, a3: Arg) -> Expr {
    Expr::TerExp(TerOp::Select, a1, a2, a3)
}

#[derive(Debug)]
pub struct Stmt {
    pub var: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum ExitOp {
    JC(Var, Label, Label),
    JMP(Label),
    RET
}

pub fn jc(cond: Var, label1: Label, label2: Label) -> ExitOp {
    ExitOp::JC(cond, label1, label2)
}

pub fn jmp(label: Label) -> ExitOp {
    ExitOp::JMP(label)
}

pub fn ret() -> ExitOp {
    ExitOp::RET
}

#[derive(Debug)]
pub enum BBBody {
    SeqBB (Vec<Stmt>),
    PipeBB (Vec<Stmt>, Var)
}

#[derive(Debug)]
pub struct BB {
    pub prevs: Vec<Label>, 
    pub body: BBBody,
    pub exit: ExitOp
}

pub type CFG = HashMap<Label, BB>;

#[derive(Debug)]
pub struct GatedSSAIR {
    pub name: String,
    pub start: Label,
    pub params: Vec<Var>,
    pub cfg: CFG,
    pub returns: Vec<Var>,
}

// Only dataflow dependency is currently supported.
// TODO: other types of dependency should be supported
// e.g., Anti/Output, May/Must/...
#[derive(Debug)]
pub enum DepType {
    Intra,
    Carried(i32),
    InterBB,
}

#[derive(Debug)]
pub struct Edge {
    pub var: Var,
    pub dep_type: DepType,
}

#[derive(Debug)]
pub struct DFGNode<SCHED> {
    pub stmt: Stmt,
    pub prevs: Vec<Edge>,
    pub succs: Vec<Edge>,
    pub sched: SCHED,
}

pub type DFG<SCHED> = HashMap<Var, DFGNode<SCHED>>;

#[derive(Debug)]
pub enum DFGBBBody<SCHED, II> {
    Seq(DFG<SCHED>),
    Pipe(DFG<SCHED>, Var, II)
}

#[derive(Debug)]
pub struct DFGBB<SCHED, II> {
    pub prevs: Vec<Label>, 
    pub body: DFGBBBody<SCHED, II>,
    pub exit: ExitOp
}

pub type CDFG<SCHED, II> = HashMap<Label, DFGBB<SCHED, II>>;

// T is the additional info for DFG, U is the additional info for 
#[derive(Debug)]
pub struct GenCDFGIR<SCHED: fmt::Debug, II: fmt::Debug> {
    pub name: String,
    pub start: Label,
    pub params: Vec<Var>,
    pub cdfg: CDFG<SCHED, II>,
    pub returns: Vec<Var>
}

#[derive(Debug)]
pub struct Sched {
    pub sched: i32,
}

pub type CDFGIR = GenCDFGIR<(), ()>;
pub type SchedCDFGIR = GenCDFGIR<Sched, i32>;

#[derive(Debug, Clone)]
pub struct VVar {
    pub name: String,
    pub bits: i32,
    pub idx: Option<i32>,
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

#[derive(Debug, Clone)]
pub enum VExpr {
    UnExp(UnOp, Rc<VExpr>),
    BinExp(BinOp, Rc<VExpr>, Rc<VExpr>),
    TerExp(TerOp, Rc<VExpr>, Rc<VExpr>, Box<VExpr>),
    Const(i32),
    Var(VVar)
}

impl fmt::Display for VExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VExpr::UnExp(op, e) => {
                match op {
                    UnOp::Neg => write!(f, "-({})", e),
                    UnOp::Not => write!(f, "!({})", e),
                }
            },
            VExpr::BinExp(op, e1, e2) => {
                match op {
                    BinOp::Mu => panic!("Mu is not supported in VerilogIR"),
                    BinOp::Ita => panic!("Ita is not supported in VerilogIR"),
                    _ => write!(f, "({} {} {})", e1, op, e2)
                }
            },
            VExpr::TerExp(op, e1, e2, e3) => {
                match op {
                    TerOp::Select => write!(f, "({} ? {} : {})", e1, e2, e3)
                }
            },
            VExpr::Const(n) => write!(f, "{}", n),
            VExpr::Var(v) => write!(f, "{}", v)
        }
    }
}

#[derive(Debug)]
pub struct VAssign {
    pub lhs: VVar,
    pub rhs: VExpr
}

#[derive(Debug)]
pub enum IOType {
    Input, OutputReg,
}

impl fmt::Display for IOType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IOType::Input => write!(f, "input"),
            IOType::OutputReg => write!(f, "output reg")
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
pub struct VerilogIR {
    pub name: String,
    pub localparams: Vec<(VVar, i32)>,
    pub io_signals: Vec<(VVar, IOType)>,
    pub regs: Vec<VVar>,
    pub wires: Vec<VAssign>,
    pub always: Vec<VAlways>,
}