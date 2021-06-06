module top(
    clk,
    rst_n,
    start,
    finish,
    n,
    res
);
    input clk;
    input rst_n;
    input start;
    output finish;
    input [9:0] n;
    output [31:0] res;

    wire arr_clk;
    wire arr_read_en;
    wire [9:0] arr_read_addr;
    wire [31:0] arr_read_val;
    wire arr_write_en;
    wire [9:0] arr_write_addr;
    wire [31:0] arr_write_val;

    bram_2p m0(arr_clk, arr_read_en, arr_read_addr, arr_read_val, arr_write_en, arr_write_addr, arr_write_val);
    sum_of_array m1(clk, rst_n, start, finish, n, res, arr_clk, arr_read_en, arr_read_addr, arr_read_val, arr_write_en, arr_write_addr, arr_write_val);
endmodule