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
loop_odd1_tmp <- mult(3, loop_cur0);
loop_odd1 <- plus(loop_odd1_tmp, 1);
loop_mod1 <- mod(loop_cur0, 2);
loop_eq1 <- eq(loop_mod1, 0);
loop_tmp <- select(loop_eq1, loop_even1, loop_odd2);

// loop_cur2 <- select(loop_tmp % 2 == 0, loop_tmp / 2, 3 * loop_tmp + 1)
loop_even2 <- div(loop_tmp, 2);
loop_odd2_tmp <- mult(3, loop_tmp);
loop_odd2 <- plus(loop_odd2_tmp, 1);
loop_mod2 <- mod(loop_tmp, 2);
loop_eq2 <- eq(loop_mod2, 0);
loop_cur2 <- select(loop_eq2, loop_even2, loop_odd2);

loop_cur0 <- cur1;
loop_cur1 <- loop_cur2;
loop_cond <- gt(loop_cur1, 1);
```

```
c(x) = if x % 2 = 0 then x / 2 else 3 * x + 1
c(c(x)) = c(if x % 2 = 0 then x / 2 else 3 * x + 1)
if x % 2 = 0 then c(x / 2) else c(3 * x + 1)
= if x % 2 = 0 then c(x / 2) else c(3 * x + 1)
= if x % 2 = 0 then 
    if (x / 2) % 2 = 0 then (x / 2) / 2  else 3 * (x / 2) + 1 
    else (3 * x + 1) % 2 == 0 then (3 * x + 1) / 2 else 3 * (3 * x + 1) + 1
= if x % 4 == 0 then x / 4
  else if x % 2 == 0 then 3 * (x / 2) + 1
  else (3 * x + 1) / 2
```

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
jmp(init_test0, LOOP);

LOOP(INIT1):
cur0 <- mu(init_cur0, loop_cur0);
cur1 <- mu(init_cur1, loop_cur1);
step <- mu(init_step0, loop_step);
loop_step <- plus(step, 1);

// loop_tmp <- select(x % 4 == 0, x / 4, select(x % 2 == 0, 3 (x / 2) + 1, (3 * x + 1) / 2))
loop_if_e1 <- div(cur0, 4);
loop_if_e2_tmp1 <- div(cur0, 2);
loop_if_e2_tmp2 <- mult(3, loop_if_e2_tmp1);
loop_if_e2 <- plus(loop_if_e2_tmp2, 1);
loop_if_e3_tmp1 <- mult(3, cur0);
loop_if_e3_tmp2 <- plus(loop_if_e3_tmp1, 1);
loop_if_e3 <- div(loop_if_e3_tmp2, 2);
loop_if_else_cond <- mod_(cur0, 2);
loop_if_else <- select(loop_if_else_cond, loop_if_e2, loop_if_e3);
loop_if_cond <- mod_(x, 4);
loop_if <- select(loop_if_cond, loop_if_e1, loop_if_else);

loop_cur0 <- loop_cur1;
loop_cur1 <- loop_if;
loop_cond <- gt(cur1, 1);
```