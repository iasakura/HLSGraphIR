# Collatz (II = 1)

```rust
INIT0:
init_cur0 <- copy(n);
init_step0 <- copy(0);
init_test0 <- gt(n, 1);
jc(init_test0, INIT1, FINISH);

INIT1(INIT0):
init_even <- div(init_cur0, 2);
init_odd_tmp <- mult(init_cur0, 3);
init_odd <- plus(init_odd_tmp, 1);
init_mod <- mod_(init_cur0, 2);
init_eq1 <- eq(init_mod, 0);
init_cur1 <- select(init_eq, init_even, init_odd);
init_step1 <- 1;
init_test0 <- gt(init_cur1, 1);
jc(init_test0, LOOP, FINISH);

LOOP(INIT1):
cur0 <- mu(init_cur0, loop_cur0);
cur1 <- mu(init_cur1, loop_cur1);
step <- mu(init_step1, loop_step);
loop_step <- plus(step, 1);

// loop_tmp <- select(loop_cur0 % 2 == 0, loop_cur0 / 2, 3 * loop_cur0 + 1)
loop_even1 <- div(loop_cur0, 2);
loop_odd1_tmp <- mult(3, loop_cur0)
loop_odd1 <- plus(loop_odd1_tmp, 1);
loop_mod1 <- mod(loop_cur0, 2);
loop_eq1 <- eq(loop_mod1, 0);
loop_tmp <- select(loop_eq1, loop_even1, loop_odd2)

// loop_cur2 <- select(loop_tmp % 2 == 0, loop_tmp / 2, 3 * loop_tmp + 1)
loop_even2 <- div(loop_tmp, 2);
loop_odd2_tmp <- mult(3, loop_tmp)
loop_odd2 <- plus(loop_odd2_tmp, 1);
loop_mod2 <- mod(loop_tmp, 2);
loop_eq2 <- eq(loop_mod2, 0);
loop_cur2 <- select(loop_eq2, loop_even2, loop_odd2);

loop_cur0 <- cur1;
loop_cur1 <- loop_cur2;
loop_cond <- gt(loop_cur1, 1);
```