#[macro_use]
mod dsl;
mod types;

fn main() {
    let ir = loop_ir!{
        collatz(n) {
            init {
                cur0 := copy(n);
                step0 := copy(0);
                test0 := eq(cur0, 0)
            }
            while (mu(test0, test1)) {
                cur1 := mu(cur0, cur2);
                step1 := mu(step0, step2);
                tmp1 := div(cur1, 2);
                tmp2 := plus(cur1, 3);
                tmp3 := plus(tmp2, 1);
                tmp4 := mod_(cur1, 2);
                tmp5 := eq(tmp4, 0);
                cur2 := select(tmp5, tmp1, tmp3);
                step2 := plus(step1, 1);
                test1 := eq(cur1, 0)
            }
            exit {

            }
        }
    };
    println!("{:?}\n", ir)
}
