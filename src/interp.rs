use crate::types::*;
use std::collections::HashMap;

fn interp_arg(arg: &Arg, env: &HashMap<String, i32>) -> i32 {
    match arg {
        Arg::Val(i) => *i,
        Arg::Var(v) => match env.get(&v.name) {
            Some (v) => *v,
            None => 1234567
        }
    }
}

fn interp_unop(op: &UnOp, arg: &Arg, env: &HashMap<String, i32>, _is_first: bool) -> i32 {
    match op {
        UnOp::Neg => - interp_arg(arg, env)
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

fn interp_binop(op: &BinOp, arg1: &Arg, arg2: &Arg, env: &HashMap<String, i32>, is_first: bool) -> i32 {
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
        BinOp::Ita => if is_first { v1 } else { v2 }
    }
}

fn interp_terop(op: &TerOp, arg1: &Arg, arg2: &Arg, arg3: &Arg, env: &HashMap<String, i32>, _is_first: bool) -> i32 {
    let v1 = interp_arg(arg1, env);
    let v2 = interp_arg(arg2, env);
    let v3 = interp_arg(arg3, env);
    match op {
        TerOp::Select => if to_bool(v1) { v2 } else { v3 }
    }
}

fn interp_expr(expr: &Expr, env: &HashMap<String, i32>, is_first: bool) -> i32 {
    match expr {
        Expr::Copy (a) => interp_arg(a, env),
        Expr::UnExp(op, a) => interp_unop(op, a, env, is_first),
        Expr::BinExp(op, a1, a2) => interp_binop(op, a1, a2, env, is_first),
        Expr::TerExp(op, a1, a2, a3) => interp_terop(op, a1, a2, a3, env, is_first),
    }
}

fn interp_stmt(stmt: &Stmt, env: &mut HashMap<String, i32>, is_first: bool) {
    env.insert(stmt.var.clone(), interp_expr(&stmt.expr, env, is_first));
}

pub fn interp_loopir(ir: &LoopIR, args: &Vec<i32>) -> HashMap<String, i32> {
    if ir.params.len() != args.len() {
        panic!()
    }
    let mut env : HashMap<String, i32> = HashMap::new();

    for (param, arg) in ir.params.iter().zip(args.iter()) {
        env.insert(param.name.clone(), *arg);
    }

    for stmt in &ir.init_body {
        interp_stmt(stmt, &mut env, true);
    }

    let mut is_first = true;
    while to_bool(interp_expr(&ir.cond, &mut env, is_first)) {
        for stmt in &ir.while_body {
            interp_stmt(stmt, &mut env, is_first);
        }
        is_first = false;
    }

    for stmt in &ir.exit_body {
        interp_stmt(&stmt, &mut env, is_first);
    }

    env
}