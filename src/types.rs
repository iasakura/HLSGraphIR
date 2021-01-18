use std::collections::HashMap;

#[derive(Debug)]
pub struct Var {
    pub name: String
}

pub fn var_from_str(s: &str) -> Var {
    Var {name : String::from(s)}
}

pub fn var_from_string(s: String) -> Var {
    Var {name : s}
}

pub type Label = String;

#[derive(Debug)]
pub enum Arg {
    Var(Var),
    Val(i32)
}

#[derive(Debug)]
pub enum UnOp {
    Neg,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
pub struct LoopIR {
    pub name: String,
    pub start: Label,
    pub params: Vec<Var>,
    pub cfg: CFG,
    pub returns: Vec<Var>,
}

// WIP
#[derive(Debug)]
pub struct DFG {}

#[derive(Debug)]
pub struct LoopDFG {
    name: String,
    params: Vec<Var>,
    cond: Expr,
    body: DFG,
}

#[derive(Debug)]
pub struct LoopSched {
    name: String,
    params: Vec<Var>,
    cond: (Expr, Expr),
    ii: i32,
    body: Vec<Vec<Stmt>>
}
