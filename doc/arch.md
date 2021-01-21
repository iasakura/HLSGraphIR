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

- start