use std::fmt;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Type {
    pub bits: u32,
    pub signed: bool,
}

pub fn int(bits: u32) -> Type {
    Type {bits, signed: true}
}

pub fn uint(bits: u32) -> Type {
    Type {bits, signed: false}
}

#[derive(Debug, Clone)]
pub struct Val {
    pub val: i32,
    pub type_: Type,
}

pub fn val(val: i32, type_: Type) -> Val {
    Val {val, type_}
}

pub const TRUE: Val = Val {val: 1, type_: Type {bits: 1, signed: false}};
pub const FALSE: Val = Val {val: 0, type_: Type {bits: 1, signed: false}};

pub type Label = String;
pub fn label(s: &str) -> Label {
    s.to_string()
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

    EQ,
    LT,
    GT,
    LE,
    GE,

    And,
    Or,

    Lshift,
    Rshift,

    Mu,
}

pub fn is_bool_op(op: &BinOp) -> bool {
    match op {
        BinOp::EQ | BinOp::LT | BinOp::GT | BinOp::LE | BinOp::GE => true,
        _ => false
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
            BinOp::Mult => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),

            BinOp::EQ => write!(f, "=="),
            BinOp::LT => write!(f, "<"),
            BinOp::GT => write!(f, ">"),
            BinOp::LE => write!(f, "<="),
            BinOp::GE => write!(f, ">="),

            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),

            BinOp::Lshift => write!(f, "<<"),
            BinOp::Rshift => write!(f, ">>"),

            BinOp::Mu => write!(f, "Mu"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TerOp {
    Select
}