## IR

```rust
collatz(n) {
    starts INIT
    INIT {
        prevs ()
        seq {
            cur0 := copy(n);
            step0 := copy(0);
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
        } while (gt(cur1, 1))
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
```

## (C)DFG
```
init:
    cur0
    step0
    test0

while:
test1 ----> cond --[1]---> all

 +------------------(1)----------------------------+
 | +-----------(1)----------+                      |
 | |                        |                      |
 | +-> step1 -> step2 ------+                      |
 +---> cur1 ---> test1                             |
            +---> tmp4 -> tmp5 -+                  |
            +-----------> tmp1 ---> cur2 ----------+
            +---> tmp2 -> tmp3 -+

exit:
step
```

## schedule constraints

各 op_i に 整数値 t_i を割り当て
各 resource constrained op_i に resource id res_i を割り当て

- レイテンシ制約: t_i + L_ij <= t_j + II * Dist_ij  (forall i j, op_i -> op_j)
- タイミング制約: t_i + 1 <= t_j (forall forward dependency path p: (op_i, ... ,op_j), p cannot execute in 1 clock, p does not contain back edge)
- リソース制約: res_i = res_j -> t_i mod II != t_j mod II

 タイミング制約は forward edge しか見ない (どうせ loop carried dep は同じサイクルで実行されないので。) よって DAG 上のパスしかみない。例えば組み合わせレイテンシを見るなら各 i, j に対して最長パスだけみればよくて DAG 上の最長パスになるので多項式で計算できる。

cur1 -> tmp4 -> tmp5 -> cur2 -> (1) cur1
(L = 0 とする)
t_cur1 <= t_tmp4
t_tmp4 <= t_tmp5
t_tmp5 <= t_cur2
t_cur2 <= t_cur1 + II

## DFG + schedule

```
init:
    cur0 (0)
    step0 (0)
    test0 (0)

while:
cond[0] ----> all, test1[0] --(1)--> cond

 +------------------(1)----------------------------------+
 | +-----------(1)----------------+                      |
 | |                              |                      |
 | +-> step1[0] -> step2[0] ------+                      |
 +---> cur1[0] ---> test1[0]                             |
            +--> tmp4[0] --> tmp5[1] -+                  |
            +--------------> tmp1[1] ---> cur2[1] -------+
            +--> tmp2[0] --> tmp3[1] -+

exit:
    step (0)

```

```verilog
II_counter = counter(0, 2)
reg enable[2];

if (enable[0] == 0) begin
    do ops @ 0
end else begin
    do ops @ 1
end
```

```
edge
  |  incr ii cnt                                |
  |    shift enable and prepend ii == 0 && cond |
  |    do op with prev enable                   |
         
```
