module simple_top(
    clk,
    rst_n,
    start,
    finish,
    inp,
    res
);
    input clk;
    input rst_n;
    input start;
    output finish;
    input [31:0] inp;
    output [31:0] res;

    simple simple(clk, rst_n, start, finish, inp, res);
endmodule