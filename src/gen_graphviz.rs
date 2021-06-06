use std::borrow::Cow;
use std::io;
use std::fs;
use std::str;

use crate::cdfg_ir::*;

#[derive(Clone)]
struct Edge(Var, Var, DepType);

pub trait SchedToLabel {
    fn to_label(&self) -> String;
}

impl SchedToLabel for Sched {
    fn to_label(&self) -> String {
        format!("{}", self.sched)
    }
}

impl SchedToLabel for () {
    fn to_label(&self) -> String {
        "".to_string()
    }
}

impl<'a, SCHED: SchedToLabel> dot::Labeller<'a, DFGNode<SCHED>, Edge> for DFG<SCHED> {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new("DFG").unwrap()
    }

    fn node_id(&'a self, n: &DFGNode<SCHED>) -> dot::Id<'a> {
        dot::Id::new(n.stmt.var.name.clone()).unwrap()
    }

    fn node_label(&'a self, n: &DFGNode<SCHED>) -> dot::LabelText<'a> {
        dot::LabelText::LabelStr(format!(
            "{}: {}", n.stmt.var.name ,n.sched.to_label()
        ).into())
    }

    fn edge_label<'b>(&'b self, e: &Edge) -> dot::LabelText<'b> {
        match e.2 {
            DepType::Carried(n) => dot::LabelText::LabelStr(format!("Carried({})", n).into()),
            _ => dot::LabelText::LabelStr("".into())
        }
    }

    fn edge_color<'b>(&'b self, e: &Edge) -> Option<dot::LabelText<'b>> {
        match e.2 {
            DepType::Carried(_) => Some(dot::LabelText::LabelStr("red".into())),
            _ => None
        }
    }
}

impl<'a, SCHED: Clone + SchedToLabel> dot::GraphWalk<'a, DFGNode<SCHED>, Edge> for DFG<SCHED> {
    fn nodes(&self) -> dot::Nodes<'a, DFGNode<SCHED>> {
        let nodes = self.iter().map(|(_, stmt)| stmt.clone() ).collect::<Vec<_>>();
        Cow::Owned(nodes)
    }

    fn edges(&'a self) -> dot::Edges<'a, Edge> {
        let es = self.iter()
            .map(|(v, stmt)| {
                let v = v.clone();
                get_deps(&stmt.stmt, self).into_iter().map(move |dep| Edge(dep.0, v.clone(), dep.1))
            })
            .flatten()
            .filter(|Edge(_, _, dep) | match dep {
                DepType::InterBB => false,
                _ => true
            })
            .collect::<Vec<_>>();
        Cow::Owned(es)
    }

    fn source(&'a self, edge: &Edge) -> DFGNode<SCHED> {
        self.get(&edge.0).unwrap().clone()
    }

    fn target(&'a self, edge: &Edge) -> DFGNode<SCHED> {
        self.get(&edge.1).unwrap().clone()
    }
}

pub fn gen_graphviz<SCHED: Clone + SchedToLabel>(graph: &DFG<SCHED>, stream: &mut impl io::Write) {
    let mut v = Vec::<u8>::new();
    dot::render(graph, &mut v).unwrap();
    // Add constraint=false for ignoring carried dependency edges for determining layout
    let s = str::from_utf8(&v[..]).unwrap().replace("\"Carried(1)\"", "\"Carried(1)\", constraint=false");
    stream.write_all(&s.as_bytes()).unwrap()
}

pub fn gen_graphviz_from_dfg<SCHED: Clone + SchedToLabel>(graph: &DFG<SCHED>, filename: &str) {
    let mut stream = io::BufWriter::new(fs::File::create(filename).unwrap());
    gen_graphviz(graph, &mut stream);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir_basic::{int, val};
    use crate::dfg;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn gen_graphviz_test1() {
        init();

        let init_cur0 = &var("init_cur0", int(32));
        let init_cur1 = &var("init_cur1", int(32));
        let init_step1 = &var("init_step1", int(32));

        let cur0 = &var("cur0", int(32));
        let cur1 = &var("cur1", int(32));
        let step = &var("step", int(32));
        let loop_step = &var("loop_step", int(32));

        let loop_even1 = &var("loop_even1", int(32));
        let loop_odd1_tmp = &var("loop_odd1_tmp", int(32));
        let loop_odd1 = &var("loop_odd1", int(32));
        let loop_mod1 = &var("loop_mod1", int(32));
        let loop_eq1 = &var("loop_eq1", int(32));
        let loop_tmp = &var("loop_tmp", int(32));

        let loop_even2 = &var("loop_even2", int(32));
        let loop_odd2_tmp = &var("loop_odd2_tmp", int(32));
        let loop_odd2 = &var("loop_odd2", int(32));
        let loop_mod2 = &var("loop_mod2", int(32));
        let loop_eq2 = &var("loop_eq2", int(32));
        let loop_cur2 = &var("loop_cur2", int(32));

        let loop_cur0 = &var("loop_cur0", int(32));
        let loop_cur1 = &var("loop_cur1", int(32));
        let loop_cond = &var("loop_cond", int(32));

        let dfg = dfg!{
            cur0 <- mu(init_cur0, loop_cur0);
            cur1 <- mu(init_cur1, loop_cur1);
            step <- mu(init_step1, loop_step);
            loop_step <- plus(step, val(1, int(32)));
            // loop_tmp <- select(cur0 % 2 == 0, cur0 / 2, 3 * cur0 + 1)
            loop_even1 <- div(cur0, val(2, int(32)));
            loop_odd1_tmp <- mult(val(3, int(32)), cur0);
            loop_odd1 <- plus(loop_odd1_tmp, val(1, int(32)));
            loop_mod1 <- mod_(cur0, val(2, int(32)));
            loop_eq1 <- eq(loop_mod1, val(0, int(32)));
            loop_tmp <- select(loop_eq1, loop_even1, loop_odd1);
            // loop_cur2 <- select(loop_tmp % 2 == 0, loop_tmp / 2, 3 * loop_tmp + 1)
            loop_even2 <- div(loop_tmp, val(2, int(32)));
            loop_odd2_tmp <- mult(val(3, int(32)), loop_tmp);
            loop_odd2 <- plus(loop_odd2_tmp, val(1, int(32)));
            loop_mod2 <- mod_(loop_tmp, val(2, int(32)));
            loop_eq2 <- eq(loop_mod2, val(0, int(32)));
            loop_cur2 <- select(loop_eq2, loop_even2, loop_odd2);

            loop_cur0 <- copy(cur1);
            loop_cur1 <- copy(loop_cur2);
            loop_cond <- gt(loop_cur1, val(1, int(32)));
        };
        gen_graphviz_from_dfg(&dfg, "test/collatz_ii1.dot");
    }
}