#[macro_use]
extern crate hls_graph_ir;

use hls_graph_ir::dsl;
use hls_graph_ir::types;
use hls_graph_ir::interp;

fn main() {
    let ir = loop_ir!{
        collatz(n) {
            starts INIT
            INIT {
                prevs ()
                seq {
                    cur0 := copy(n);
                    step0 := copy(0);
                    test0 := gt(n, 1)
                }
                exit (jc(test0, LOOP, EXIT))
            },
            LOOP {
                prevs (INIT)
                loop {
                    cur1 := mu(cur0, cur2);
                    step1 := mu(step0, step2);
                    tmp1 := div(cur1, 2);
                    tmp2 := mult(cur1, 3);
                    tmp3 := plus(tmp2, 1);
                    tmp4 := mod_(cur1, 2);
                    tmp5 := eq(tmp4, 0);
                    cur2 := select(tmp5, tmp1, tmp3);
                    step2 := plus(step1, 1);
                    test1 := gt(cur1, 1)
                } while (test1)
                exit (jmp(EXIT))
            },
            EXIT {
                prevs (INIT, LOOP)
                seq {
                    step := ita(step0, step2)
                }
                exit (ret())
            }
            returns (step)
        }
    };
    println!("{:?}\n", ir);
    println!("collatz(1) = {:?}", interp::interp_loopir(&ir, &vec![1]));
    println!("collatz(10) = {:?}", interp::interp_loopir(&ir, &vec![10]));
    println!("collatz(20) = {:?}", interp::interp_loopir(&ir, &vec![20]));
    println!("collatz(30) = {:?}", interp::interp_loopir(&ir, &vec![30]));
}
