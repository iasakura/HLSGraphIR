# コード生成と生成されるアーキテクチャ

- gated SSA -> CDFG -> verilog

## トップジュール

- clk, rst_n, start, finish, args, ret (tuple)

args は wire

## CFG

- 状態機械になる
- 終了状態を表す特殊な状態 FIN を追加

- rst_n で初期化
- en で開始、fin が来ると状態遷移
    - fin を観測した clock で次の BB の初期化もやる

### I/F
- BB_en: out
- BB_done: in

### 内部状態
- enum state { BB1, BB2, ... }
    - cur_state/prev_state
        - prev_state は eta ノードのために必要

## BB
以下の I/F を持つモジュールになる

- en
- done

レイテンシは可変とする


### External module

```rust
port FIFO {
    read() -> i32 {
        latency: Variable,
    }
}

cdfg sample {
    starts INIT;
    // Arguments are either scalar or port
    params (n, port FIFO);
    returns ret;
    resources {
        // internal module instantiation
        module add_0(input clk, input )
        module add_0 : ADDR {
            call : (int(32), int(32)) -> int(32) {
                latency: Fixed(0), // combinatorial logic
                initiation_interval: 1
            }
        }
        module bram_0 : BRAM_2P {
            read: (int(32)) -> int(32) {
                timing: {
                    latency: Fixed(1), // sequential logic
                    initiation_interval: 1
                }
            }
            write: (int(32), int(32)) -> int(32) {
                timing: {
                    latency: Fixed(2),
                    initiation_interval: 1
                }
            }
        }
    }
    cdfg {
        INIT: {

        } exit(jc(test0, LOOP, EXIT))
        LOOP(INIT) {
            sum = mu(0, sum);
            sum_next = call add_0(i, sum)
        } exit(jmp(EXIT))
        EXIT(INIT, LOOP) {

        } exit(ret)
    }
```