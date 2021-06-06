module top(
    clk,
    rst_n,
    start,
    finish,
    n,
    res,
    arr_clk,
    arr_read_en,
    arr_read_addr_arg,
    arr_read_val_ret,
    arr_write_en,
    arr_write_addr_arg,
    arr_write_val_arg
);
    input clk;
    input rst_n;
    input start;
    output finish;
    input [9:0] n;
    output [31:0] res;
    output arr_clk;
    output arr_read_en;
    output [9:0] arr_read_addr_arg;
    input [31:0] arr_read_val_ret;
    output arr_write_en;
    output [9:0] arr_write_addr_arg;
    output [31:0] arr_write_val_arg;

    sum_of_array_vivado sum_of_array_vivado(clk, rst_n, start, finish, n, res, arr_clk, arr_read_en, arr_read_addr_arg, arr_read_val_ret, arr_write_en, arr_write_addr_arg, arr_write_val_arg);
endmodule