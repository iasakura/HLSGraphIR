```
collatz(n) = 
```

```python
def collatz(n):
    cur = n
    step = 1
    while cur != 1:
        if cur % 2 == 0:
            cur = cur // 2
        else:
            cur = 3 * cur + 1
        step += 1
    return step
```

```
loop collatz(n)
    init {
        cur0 := n,
        step0 := 1,
        cond0 := cur != 1,
    }
    loop cond0 {
        cur1 := mu(cur0, cur2)
        step1 := mu(step0, step2)
        tmp1 := cur1 / 2
        tmp2 := cur1 * 3
        tmp3 := tmp2 + 1
        tmp4 := cur1 % 2
        tmp5 := tmp4 == 0
        cur2 := select(tmp5, tmp1, tmp3)
        step2 := step1 + 1
    }
    exit {
        step := ita(step0, step2)
    }
    returns (step)
```