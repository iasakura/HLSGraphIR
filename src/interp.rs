use crate::types::*;
use std::collections::HashMap;

type Env = HashMap<String, i32>;

fn interp_var(var: &Var, env: &Env) -> i32 {
    match env.get(&var.name) {
        Some (v) => *v,
        None => 1234567
    }
}

fn interp_arg(arg: &Arg, env: &Env) -> i32 {
    match arg {
        Arg::Val(i) => *i,
        Arg::Var(v) => interp_var(v, env)
    }
}

fn interp_unop(op: &UnOp, arg: &Arg, env: &Env, _prev_index: i32, _is_first: bool) -> i32 {
    match op {
        UnOp::Neg => - interp_arg(arg, env),
        UnOp::Not => to_int(!to_bool(interp_arg(arg, env)))
    }
}

fn to_int(b: bool) -> i32 {
    if b {
        1
    } else {
        0
    }
}

fn to_bool(i: i32) -> bool {
    if i == 0 {
        false
    } else {
        true
    }
}

fn interp_binop(op: &BinOp, arg1: &Arg, arg2: &Arg, env: &Env, prev_index: i32, is_first: bool) -> i32 {
    let v1 = interp_arg(arg1, env);
    let v2 = interp_arg(arg2, env);
    match op {
        BinOp::Plus => v1 + v2,
        BinOp::Minus => v1 - v2,
        BinOp::Mult => v1 * v2,
        BinOp::Div => v1 / v2,
        BinOp::Mod => v1 % v2,

        BinOp::Eq => to_int(v1 == v2),
        BinOp::LT => to_int(v1 < v2),
        BinOp::GT => to_int(v1 > v2),
        BinOp::LE => to_int(v1 <= v2),
        BinOp::GE => to_int(v1 >= v2),

        BinOp::And => to_int(to_bool(v1) && to_bool(v2)),
        BinOp::Or => to_int(to_bool(v1) || to_bool(v2)),

        BinOp::Mu => if is_first { v1 } else { v2 }
        BinOp::Ita => if prev_index == 0 { v1 } else if prev_index == 1 { v2 } else { panic!("TODO: support prev_index more than 2.") }
    }
}

fn interp_terop(op: &TerOp, arg1: &Arg, arg2: &Arg, arg3: &Arg, env: &Env, _prev_index: i32, _is_first: bool) -> i32 {
    let v1 = interp_arg(arg1, env);
    let v2 = interp_arg(arg2, env);
    let v3 = interp_arg(arg3, env);
    match op {
        TerOp::Select => if to_bool(v1) { v2 } else { v3 }
    }
}

fn interp_expr(expr: &Expr, env: &Env, prev_index: i32, is_first: bool) -> i32 {
    match expr {
        Expr::Copy (a) => interp_arg(a, env),
        Expr::UnExp(op, a) => interp_unop(op, a, env, prev_index, is_first),
        Expr::BinExp(op, a1, a2) => interp_binop(op, a1, a2, env, prev_index, is_first),
        Expr::TerExp(op, a1, a2, a3) => interp_terop(op, a1, a2, a3, env, prev_index, is_first),
    }
}

fn interp_stmt(stmt: &Stmt, env: &mut Env, prev_index: i32, is_first: bool) {
    // println!("{:?}", stmt);
    env.insert(stmt.var.clone(), interp_expr(&stmt.expr, env, prev_index, is_first));
}

fn get_bb<'a>(cfg: &'a CFG, l: &Label) -> &'a BB {
    cfg.get(l).expect(&format!("{} not found", l))
}

fn interp_seq(ss: &Vec<Stmt>, prev_index: i32, env: &mut Env) {
    for stmt in ss {
        interp_stmt(stmt, env, prev_index, true);
    }
}

fn interp_pipe(ss: &Vec<Stmt>, cond: &Var, prev_index: i32, env: &mut Env) {
    let mut is_first = true;
    loop {
        for stmt in ss {
            interp_stmt(stmt, env, prev_index, is_first);
        }
        is_first = false;
        if !to_bool(interp_var(cond, env)) {
            break;
        }
    }
}

fn interp_bb_body(bb_body: &BBBody, prev_index: i32, env: &mut Env) {
    match bb_body {
        BBBody::SeqBB(ss) => interp_seq(ss, prev_index, env),
        BBBody::PipeBB(ss, cond) => interp_pipe(ss, cond, prev_index, env)
    }
}

fn interp_bb<'a>(bb: &'a BB, prev_label: &Label, env: &mut Env) -> Option<&'a Label> {
    let prev_index = if prev_label == "" {
        -1
    } else {
        bb.prevs.iter().position(|l| l == prev_label).expect(&format!("{} not found", prev_label)) as i32
    };
    interp_bb_body(&bb.body, prev_index, env);
    match &bb.exit {
        ExitOp::JC(cond, l1, l2) => {
            if to_bool(interp_var(&cond, &env)) {
                Some(&l1)
            } else {
                Some(&l2)
            }
        },
        ExitOp::JMP(l) => {
            Some(&l)
        },
        ExitOp::RET => {
            None
        }
    }
}

pub fn interp_loopir(ir: &GatedSSAIR, args: &Vec<i32>) -> Vec<i32> {
    if ir.params.len() != args.len() {
        panic!()
    }
    let mut env : Env = HashMap::new();

    for (param, arg) in ir.params.iter().zip(args.iter()) {
        env.insert(param.name.clone(), *arg);
    }

    let mut cur_label = &ir.start;
    let mut bb = get_bb(&ir.cfg, &cur_label);
    let mut prev_label = &String::from("");
    loop {
        let new_label = match interp_bb(&bb, prev_label, &mut env) {
            Some(l) => l,
            None => break
        };
        bb = get_bb(&ir.cfg, new_label);
        prev_label = cur_label;
        cur_label = new_label;
    }

    ir.returns.iter()
        .map( |name| { *env.get(&name.name).expect(&format!("{} not found", name.name)) } )
        .collect::<Vec<_>>()
}