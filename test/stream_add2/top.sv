module stream_src_sim #(
    parameter C = 3'd3
) (
    clk,
    reset_n,
    val,
    n,
    valid,
    ready,
    start,
    finish
);
    input clk;
    input reset_n;
    output [31:0] val;
    input [31:0] n;
    output logic valid;
    input ready;
    input start;
    output logic finish;

    logic [31:0] mem[0:1023];
    logic [31:0] idx;
    logic [2:0] state;
    logic start_reg[0:1];
    logic [2:0] cnt;
    logic [2:0] next_cnt;
    logic [31:0] n_reg;

    localparam ST_INVALID = 0;
    localparam ST_VALID = 1;
    localparam ST_FIN = 2;

    assign val = mem[idx];
    assign next_cnt = C * cnt + 3'(mem[idx + 32'd1]);

    always @(posedge clk) begin
        if (!reset_n) begin
            idx <= 32'd0;
            state <= ST_INVALID;
            finish <= 1'd0;
            start_reg[0] <= 1'd0;
            start_reg[1] <= 1'd0;
            cnt <= 3'd0;
        end

        start_reg[0] <= start;
        start_reg[1] <= start_reg[0];

        if (reset_n && start_reg[0] && !start_reg[1]) begin
            idx <= 32'd0;
            state <= ST_INVALID;
            finish <= 1'd0;
            cnt <= 3'd0;
            n_reg <= n;
        end

        if (reset_n && state == ST_INVALID) begin
           if (cnt == 3'd0) begin
               valid <= 1'd1;
               state <= ST_VALID;
           end else begin
               cnt <= cnt - 3'd1;
           end
        end

        if (reset_n && state == ST_VALID) begin
            if (ready) begin
                idx <= idx + 32'd1;
                if (idx + 32'd1 == n) begin
                    valid <= 1'd0;
                    state <= ST_FIN;
                end else if (next_cnt == 3'd0) begin
                    cnt <= 3'd0;
                    state <= ST_VALID;
                end else begin
                    cnt <= next_cnt - 3'd1;
                    valid <= 1'd0;
                    state <= ST_INVALID;
                end
            end else begin

            end
        end

        if (reset_n && state == ST_FIN) begin
            finish <= 1'd1;
        end
    end

    // initial $readmemh("input.hex", mem);
endmodule

module stream_sink_sim(
    clk,
    reset_n,
    val,
    n,
    valid,
    ready,
    start,
    finish
);
    input clk;
    input reset_n;
    input [31:0] val;
    input [31:0] n;
    input valid;
    output logic ready;
    input start;
    output logic finish;

    localparam ST_RUN = 0;
    localparam ST_FIN = 1;

    logic [31:0] mem[0:1023];
    logic [2:0] state;
    logic [31:0] idx;
    logic start_reg;

    always @(posedge clk) begin
        if (!reset_n) begin
            idx <= 32'd0;
            state <= ST_RUN;
        end

        start_reg <= start;

        if (reset_n && start && !start_reg) begin
            state <= ST_RUN;
            idx <= 32'd0;
            ready <= 1'd1;
            finish <= 1'd0;
        end

        if (reset_n && state == ST_RUN) begin
            ready <= 1'd1;
            if (valid) begin
                mem[idx] <= val;
                idx <= idx + 32'd1;

                if (idx + 32'd1 == n) begin
                    state <= ST_FIN;
                end else begin
                    state <= ST_RUN;
                end
            end
        end

        if (reset_n && state <= ST_FIN) begin
            finish <= 1'd1;
        end
    end

    // initial begin
    //     wait(!reset_n);
    //     wait(finish);
    //     $writememh("output.hex", mem);
    // end
endmodule

module top(
    clk,
    rst_n,
    start,
    finish,
    n
);
    input clk;
    input rst_n;
    input start;
    output finish;
    input [31:0] n;

    wire [31:0] val_src2;
    wire valid_src2;
    wire ready_src2;
    wire finish_src2;

    wire [31:0] val_src1;
    wire valid_src1;
    wire ready_src1;
    wire finish_src1;

    wire finish_add;

    wire [31:0] val_sink;
    wire valid_sink;
    wire ready_sink;
    wire finish_sink;

    assign finish = finish_src1 && finish_src2 && finish_add && finish_sink;

    stream_src_sim#(.C(3'd5)) stream_src1(
        clk,
        rst_n,
        val_src1,
        n,
        valid_src1,
        ready_src1,
        start,
        finish_src1
    );

    stream_src_sim#(.C(3'd3)) stream_src2(
        clk,
        rst_n,
        val_src2,
        n,
        valid_src2,
        ready_src2,
        start,
        finish_src2
    );

    stream_sink_sim stream_sink(
        clk,
        rst_n,
        val_sink,
        n,
        valid_sink,
        ready_sink,
        start,
        finish_sink
    );

    /* verilator lint_off PINMISSING */
    stream_add2 stream_add2(
        .clk(clk),
        .rst_n(rst_n),
        .start(start),
        .finish(finish_add),
        .n(n),
        // stream1_clk,
        .stream1_get_en(ready_src1),
        .stream1_get_done(valid_src1),
        .stream1_get_val_ret(val_src1),
        // stream1_put_en,
        // stream1_put_done,
        // stream1_put_val_arg,
        // stream1_put_dummy_ret_ret,
        // stream2_clk,
        .stream2_get_en(ready_src2),
        .stream2_get_done(valid_src2),
        .stream2_get_val_ret(val_src2),
        // stream2_put_en,
        // stream2_put_done,
        // stream2_put_val_arg,
        // stream2_put_dummy_ret_ret,
        // output_clk,
        // output_get_en,
        // output_get_done,
        // output_get_val_ret,
        .output_put_en(valid_sink),
        .output_put_done(ready_sink),
        .output_put_val_arg(val_sink)
        // output_put_dummy_ret_ret
    );
endmodule

// module simulation;

//     localparam STEP = 10;

//     logic clk;
//     logic rst_n;
//     logic start;
//     logic finish;
//     logic [31:0] n;

//     top top(
//         .clk(clk),
//         .rst_n(rst_n),
//         .start(start),
//         .finish(finish),
//         .n(n)
//     );

//     always begin
//         clk = 0; #(STEP / 2);
//         clk = 1; #(STEP / 2);
//     end

//     initial begin
//         #(STEP / 2); rst_n = 1;
//         #(STEP*10); rst_n = 0;
//         #(STEP*10); rst_n = 1;
//         #(STEP*10);
//         n = 20; #(STEP);
//         start = 0; #(STEP * 10);
//         start = 1;
//     end

//     int idx;
//     initial begin
//         $dumpfile("stream_add2.vcd");
//         $dumpvars(0, top);

//         for (idx = 0; idx < 2; idx = idx + 1)
//             $dumpvars(0, top.stream_add2.start_reg[idx]);
//     end

// endmodule