#[derive(Debug)]
pub struct Var {
    pub name: String
}

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

    Mu
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

pub fn select(a1 : Arg, a2 : Arg, a3: Arg) -> Expr {
    Expr::TerExp(TerOp::Select, a1, a2, a3)
}

#[derive(Debug)]
pub struct Stmt {
    pub var: String,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct LoopIR {
    pub name: String,
    pub params: Vec<Var>,
    pub cond: Expr,
    pub init_body: Vec<Stmt>,
    pub while_body: Vec<Stmt>,
    pub exit_body: Vec<Stmt>,
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
