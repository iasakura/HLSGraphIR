use std::fmt;

use indexmap::map::IndexMap;
use indexmap::set::IndexSet;

use crate::ir_basic::*;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Var {
    pub name: String,
    pub type_: Type,
}

pub fn var<T: fmt::Display>(name: T, type_: Type) -> Var {
    let name = name.to_string();
    Var {name, type_}
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}'d{}", self.type_.bits, self.val)
    }
}

#[derive(Debug, Clone)]
pub enum Arg {
    Var(Var),
    Val(Val)
}

#[derive(Debug, Clone)]
pub enum Expr {
    // resource.method(args) depends deps
    Call (String, String, Vec<Arg>, Vec<(Var, DepType)>),
    Copy (Arg),
    UnExp (UnOp, Arg),
    BinExp (BinOp, Arg, Arg),
    TerExp (TerOp, Arg, Arg, Arg),
    Ita (Vec<Arg>)
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

pub fn ita2<T1: ToArg, T2: ToArg>(a1: T1, a2: T2) -> Expr {
    Expr::Ita(vec![a1.to_arg(), a2.to_arg()])
}

pub fn ita3<T1: ToArg, T2: ToArg, T3: ToArg>(a1: T1, a2: T2, a3: T3) -> Expr {
    Expr::Ita(vec![a1.to_arg(), a2.to_arg(), a3.to_arg()])
}

pub fn ita4<T1: ToArg, T2: ToArg, T3: ToArg, T4: ToArg>(a1: T1, a2: T2, a3: T3, a4: T4) -> Expr {
    Expr::Ita(vec![a1.to_arg(), a2.to_arg(), a3.to_arg(), a4.to_arg()])
}


#[macro_export]
macro_rules! call {
    ($mod_name:ident, $meth_name:ident, [$( $arg:expr ),*], [$($dep:expr),*])  => {
        crate::cdfg_ir::Expr::Call( 
            stringify!($mod_name).to_string(),
            stringify!($meth_name).to_string(),
            vec![
                $(
                    $arg.to_arg()
                ),*
            ],
            vec![
                $(
                    // For cloning var
                    { let t = $dep; (t.0.clone(), t.1) }
                ),*
            ]
        )
    };
}


gen_op_def!{unop( neg, UnOp::Neg )}
gen_op_def!{unop( not, UnOp::Not )}

gen_op_def!{binop( mu, BinOp::Mu )}
gen_op_def!{binop( plus, BinOp::Plus )}
gen_op_def!{binop( minus, BinOp::Minus )}
gen_op_def!{binop( mult, BinOp::Mult )}
gen_op_def!{binop( div, BinOp::Div )}
gen_op_def!{binop( mod_, BinOp::Mod )}
gen_op_def!{binop( lshift, BinOp::Lshift )}
gen_op_def!{binop( rshift, BinOp::Rshift )}
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
        Expr::Call(_, _, args, deps) => {
            args.into_iter().flat_map(|a| get_var_of_arg(a))
                .map(|v| (v.clone(), dep_of(&v, dfg)))
                .chain(deps.into_iter().map(|v| v.clone()))
                .collect::<Vec<_>>()
        }
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
                _ => 
                    get_var_of_arg(a1).iter()
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
        Expr::Ita(args) => {
            args.iter().flat_map(|arg| {
                get_var_of_arg(arg).map(|v| (v.clone(), dep_of(&v, dfg)))
            }).collect::<Vec<_>>()
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
        crate::cdfg_ir::to_dfg(vec![
            $( (stmt($v, $e), sched($i)) ),*
        ])
    };
    ( $( $v:ident <- $e:expr; )* ) => {
        crate::cdfg_ir::to_dfg(vec![
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

#[derive(Debug, Clone)]
pub enum Timing {
    Combinatorial,
    // latency, initiation interval
    Fixed(u32, u32),
    Variable,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Signal {
    Enable,
    Done,
    Continue
}

impl Signal {
    pub fn signal_name_suffix(&self) -> &str {
        match self {
            Signal::Enable => "en",
            Signal::Done => "done",
            Signal::Continue => "cont",
        }
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub inputs: Vec<Var>,
    pub outputs: Vec<Var>,
    pub timing: Timing,
    pub interface_signal: IndexSet<Signal>
}

#[derive(Debug, Clone)]
pub struct ResourceType {
    pub methods: IndexMap<String, Method>
}

pub type CDFG<SCHED, II> = IndexMap<Label, DFGBB<SCHED, II>>;

#[derive(Debug)]
pub struct GenCDFGModule<SCHED: fmt::Debug, II: fmt::Debug> {
    pub name: String,
    pub start: Label,
    pub params: Vec<Var>,
    // External resources
    pub ports: Vec<(String, String)>,
    pub resources: IndexMap<String, String>,
    pub cdfg: CDFG<SCHED, II>,
    pub returns: Vec<Var>
}

pub struct GenCDFGIR<SCHED: fmt::Debug, II: fmt::Debug> {
    pub resource_types: IndexMap<String, ResourceType>,
    pub module: GenCDFGModule<SCHED, II>,
}

#[derive(Debug, Clone)]
pub struct Sched {
    pub sched: u32,
}

pub fn sched(s: u32) -> Sched {
    Sched {sched: s}
}

pub type CDFGModule = GenCDFGModule<(), ()>;
pub type SchedCDFGModule = GenCDFGModule<Sched, u32>;

pub type CDFGIR = GenCDFGIR<(), ()>;
pub type SchedCDFGIR = GenCDFGIR<Sched, u32>;
