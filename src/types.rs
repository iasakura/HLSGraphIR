use std::fmt;
use std::rc::Rc;

use indexmap::map::IndexMap;

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

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var {
    pub name: String,
    pub type_: Type,
}

pub fn var<T: fmt::Display>(name: T, type_: Type) -> Var {
    let name = name.to_string();
    Var {name, type_}
}

#[derive(Debug, Clone)]
pub struct Val {
    pub val: i32,
    pub type_: Type,
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}'d{}", self.type_.bits, self.val)
    }
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

#[derive(Debug, Clone)]
pub enum Arg {
    Var(Var),
    Val(Val)
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

    Mu,
    Ita
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

            BinOp::Mu => write!(f, "Mu"),
            BinOp::Ita => write!(f, "Ita")
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TerOp {
    Select
}

#[derive(Debug, Clone)]
pub enum Expr {
    Copy (Arg),
    UnExp (UnOp, Arg),
    BinExp (BinOp, Arg, Arg),
    TerExp (TerOp, Arg, Arg, Arg)
}

pub trait ToArg {
    fn to_arg(self) -> Arg;
}

impl ToArg for Var {
    fn to_arg(self) -> Arg {
        Arg::Var(self.clone())
    }
}

impl ToArg for &Var {
    fn to_arg(self) -> Arg {
        Arg::Var(self.clone())
    }
}

impl ToArg for Val {
    fn to_arg(self) -> Arg {
        Arg::Val(self.clone())
    }
}

impl ToArg for &Val {
    fn to_arg(self) -> Arg {
        Arg::Val(self.clone())
    }
}

macro_rules! gen_op_def {
    (unop ( $name: ident, $op:expr ) ) => {
        pub fn $name<T: ToArg>(a: T) -> Expr {
            Expr::UnExp($op, a.to_arg())
        }
    };

    (binop ( $name: ident, $op: expr) ) => {
        pub fn $name<T1: ToArg, T2: ToArg>(a1: T1, a2: T2) -> Expr {
            Expr::BinExp($op, a1.to_arg(), a2.to_arg())
        }
    };
    
    (terop ( $name: ident, $op: expr) ) => {
        pub fn $name<T1: ToArg, T2: ToArg, T3: ToArg>(a1: T1, a2: T2, a3: T3) -> Expr {
            Expr::TerExp($op, a1.to_arg(), a2.to_arg(), a3.to_arg())
        }
    }
}

pub fn copy<T: ToArg>(a: T) -> Expr {
    Expr::Copy (a.to_arg())
}

gen_op_def!{unop( neg, UnOp::Neg )}
gen_op_def!{unop( not, UnOp::Not )}

gen_op_def!{binop( mu, BinOp::Mu )}
gen_op_def!{binop( ita, BinOp::Ita )}
gen_op_def!{binop( plus, BinOp::Plus )}
gen_op_def!{binop( minus, BinOp::Minus )}
gen_op_def!{binop( mult, BinOp::Mult )}
gen_op_def!{binop( div, BinOp::Div )}
gen_op_def!{binop( mod_, BinOp::Mod )}
gen_op_def!{binop( eq, BinOp::EQ )}
gen_op_def!{binop( lt, BinOp::LT )}
gen_op_def!{binop( le, BinOp::LE )}
gen_op_def!{binop( gt, BinOp::GT )}
gen_op_def!{binop( ge, BinOp::GE )}

gen_op_def!{terop( select, TerOp::Select )}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub var: Var,
    pub expr: Expr,
}

pub trait ToExpr {
    fn to_expr(self) -> Expr;
}

impl ToExpr for Expr {
    fn to_expr(self) -> Expr {
        self.clone()
    }
}

impl ToExpr for &Expr {
    fn to_expr(self) -> Expr {
        self.clone()
    }
}

pub fn stmt<T: ToExpr>(v: &Var, e: T) -> Stmt {
    Stmt {var: v.clone(), expr: e.to_expr()}
}

fn get_var_of_arg(a: &Arg) -> Option<Var> {
    match a {
        Arg::Var(v) => Some(v.clone()),
        Arg::Val(_v) => None
    }
}

fn dep_of<SCHED>(v: &Var, dfg: &DFG<SCHED>) -> DepType {
    if dfg.contains_key(v) {
        DepType::Intra
    } else {
        DepType::InterBB
    }
}

pub fn get_deps_of_expr<SCHED>(e: &Expr, dfg: &DFG<SCHED>) -> Vec<(Var, DepType)> {
    match e {
        Expr::Copy(a) | Expr::UnExp(_, a) => 
            get_var_of_arg(a).iter().map(|v| (v.clone(), dep_of(v, dfg))).collect::<Vec<_>>(),
        Expr::BinExp(op, a1, a2) => {
            match op {
                BinOp::Mu => {
                    let mut ds = get_var_of_arg(a1).iter().map(|v| (v.clone(), dep_of(v, dfg))).collect::<Vec<_>>();
                    let mut v2 = get_var_of_arg(a2).map(|v| (v.clone(), DepType::Carried(1))).into_iter().collect::<Vec<_>>();
                    ds.append(&mut v2);
                    ds
                },
                _ => get_var_of_arg(a1).iter()
                .chain(get_var_of_arg(a2).iter())
                .map(|v| (v.clone(), dep_of(v, dfg)))
                .collect::<Vec<_>>()
            }
        },
        Expr::TerExp(_, a1, a2, a3) => {
            get_var_of_arg(a1).iter()
                .chain(get_var_of_arg(a2).iter())
                .chain(get_var_of_arg(a3).iter())
                .map(|v| (v.clone(), dep_of(v, dfg)))
                .collect::<Vec<_>>()
        }
    }
}

pub fn get_deps<SCHED>(s: &Stmt, dfg: &DFG<SCHED>) -> Vec<(Var, DepType)> {
    get_deps_of_expr(&s.expr, dfg)
}

#[derive(Debug)]
pub enum ExitOp {
    JC(Var, Label, Label),
    JMP(Label),
    RET
}

pub fn jc(var: &Var, l1: Label, l2: Label) -> ExitOp {
    ExitOp::JC(var.clone(), l1, l2)
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
    PipeBB (Vec<Stmt>, Expr)
}

#[derive(Debug)]
pub struct BB {
    pub prevs: Vec<Label>, 
    pub body: BBBody,
    pub exit: ExitOp
}

pub type CFG = IndexMap<Label, BB>;

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
#[derive(Debug, Clone, Copy)]
pub enum DepType {
    Intra,
    Carried(u32),
    InterBB,
}

#[derive(Debug)]
pub struct Edge {
    pub var: Var,
    pub dep_type: DepType,
}

#[derive(Clone, Debug)]
pub struct DFGNode<SCHED> {
    pub stmt: Stmt,
    // pub prevs: Vec<Edge>,
    // pub succs: Vec<Edge>,
    pub sched: SCHED,
}

pub type DFG<SCHED> = IndexMap<Var, DFGNode<SCHED>>;

pub fn to_dfg<SCHED>(nodes: Vec<(Stmt, SCHED)>) -> DFG<SCHED> {
    let mut ret = IndexMap::new();
    for (stmt, sched) in nodes { 
        ret.insert(stmt.var.clone(), DFGNode {stmt, sched});
    }
    ret
}

#[macro_export]
macro_rules! dfg {
    ( $( $v:ident <- $e:expr, $i:expr ; )* ) => {
        crate::types::to_dfg(vec![
            $( (stmt($v, $e), sched($i)) ),*
        ])
    };
    ( $( $v:ident <- $e:expr; )* ) => {
        crate::types::to_dfg(vec![
            $((stmt($v, $e), ())),*
        ])
    };
}

#[derive(Debug)]
pub enum DFGBBBody<SCHED, II> {
    Seq(DFG<SCHED>),
    Pipe(DFG<SCHED>, II)
}

#[derive(Debug)]
pub struct DFGBB<SCHED, II> {
    pub prevs: Vec<Label>, 
    pub body: DFGBBBody<SCHED, II>,
    pub exit: ExitOp
}

pub type CDFG<SCHED, II> = IndexMap<Label, DFGBB<SCHED, II>>;

// T is the additional info for DFG, U is the additional info for 
#[derive(Debug)]
pub struct GenCDFGIR<SCHED: fmt::Debug, II: fmt::Debug> {
    pub name: String,
    pub start: Label,
    pub params: Vec<Var>,
    pub cdfg: CDFG<SCHED, II>,
    pub returns: Vec<Var>
}

#[derive(Debug, Clone)]
pub struct Sched {
    pub sched: u32,
}

pub fn sched(s: u32) -> Sched {
    Sched {sched: s}
}

pub type CDFGIR = GenCDFGIR<(), ()>;
pub type SchedCDFGIR = GenCDFGIR<Sched, u32>;

#[derive(Debug, Clone)]
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
                    BinOp::Ita => panic!("Ita is not supported in VerilogIR"),
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
gen_vop_def!{binop( vita, BinOp::Ita )}
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