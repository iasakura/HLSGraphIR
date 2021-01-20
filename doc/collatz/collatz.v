module collatz(
    clk, rst_n,
    start, finish,
    n,
    ret0
);
    input clk;
    input rst_n;
    input start;
    output finish;
    input [31:0] n;
    output reg [31:0] ret0;

    parameter ST_INIT = 2'd0;
    parameter ST_LOOP = 2'd1;
    parameter ST_EXIT = 2'd2;
    parameter ST_FIN  = 2'd3;

    reg [1:0] cur_state, prev_state;
    reg st_init_done, st_lop_done, st_exit_done;

    // state machine for CFG
    always @(posedge clk) begin
        if (!rst_n) begin
            prev_state <= ST_INIT; // DUMMY
            cur_state <= ST_INIT;
            // INITIALIZATION of ST_INIT
            st_init_cnt <= 0;
        end else begin
            if (state == ST_INIT) begin
                if (st_init_done) begin
                    if (reg_test0) begin
                        prev_state <= cur_state;
                        cur_state <= ST_LOOP;    
                        // INITIALIZATION of ST_LOOP  
                        st_loop_cnt <= 2'd0;
                        st_loop_stage_en[0] <= 1'b0;
                        st_loop_stage_en[1] <= 1'b0;
                        st_loop_while <= 1'b1;
                        st_loop_is_first <= 1'b1;
                    end else begin
                        pev_state <= cur_state;
                        state <= ST_EXIT;
                        // INITIALIZATION of ST_EXIT
                    end
                    st_init_done <= 1'b0;
                end
            end else if (state == ST_LOOP) begin
                if (st_loop_done) begin
                    prev_state <= cur_state;
                    cur_state <= ST_EXIT;
                    st_loop_done <= 1'b0;
                end
                // INITIALIZATION of ST_EXIT
            end else if (state == ST_EXIT) begin
                if (st_exit_done) begin
                    prev_state <= cur_state;
                    cur_state <= ST_FIN;
                    st_exit_fin <= 1'b0;
                end
                // FINALIZATION
            end else begin
                $display("unreachable");
                $finish;
            end
        end
    end

    assign st_init_en = cur_state == ST_INIT;
    assign st_loop_en = cur_state == ST_LOOP;
    assign st_exit_en = cur_state == ST_EXIT;
    assign finish     = cur_state == ST_FIN;

    reg [31:0] reg_cur0;
    reg [31:0] reg_step0;
    reg [31:0] reg_test0;

    wire [31:0] wire_cur0;
    wire [31:0] wire_step0;
    wire [31:0] wire_test0;
    
    assign wire_cur0 = n;
    assign wire_step0 = 32'b0;
    assign wire_test0 <= n > 32'b1;

    //
    // ST_INIT
    //
    // state in ST_INIT
    reg [0:0] st_init_cnt;

    always @(posedge clk) begin
        if (st_init_en) begin
            if (st_init_cnt == 1'b0) begin
                // compute
                reg_cur0 <= wire_cur0;
                reg_step0 <= wire_step0;
                reg_test0 <= wire_test0;
                // finish
                st_init_fin <= 1;
            end
        end
    end

    //
    // ST_LOOP
    //
    // Pipeline states in ST_LOOP
    reg [1:0] st_loop_cnt;
    reg st_loop_stage_en[1:0];
    reg st_loop_is_first;
    reg st_loop_stage_is_first[1:0];
    reg st_loop_while;

    // Pipeline registers: only used regs by other stages
    reg [31:0] reg_cur1;
    // reg [31:0] reg_step1;
    // reg [31:0] reg_tmp1;
    reg [31:0] reg_tmp2;
    // reg [31:0] reg_tmp3;
    reg [31:0] reg_tmp4;
    // reg [31:0] reg_tmp5;
    reg [31:0] reg_cur2;
    reg [31:0] reg_step2;
    // reg [31:0] reg_test1;

    // Pipeline wires
    wire [31:0] wire_cur1;
    wire [31:0] wire_step1;
    wire [31:0] wire_tmp1;
    wire [31:0] wire_tmp2;
    wire [31:0] wire_tmp3;
    wire [31:0] wire_tmp4;
    wire [31:0] wire_tmp5;
    wire [31:0] wire_cur2;
    wire [31:0] wire_step2;
    wire [31:0] wire_test1;
    wire wire_while;

    assign wire_cur1 = st_loop_stage_is_first ? reg_cur0 : reg_cur2;
    assign wire_step1 = st_loop_stage_is_first ? reg_step0 : reg_step1;
    assign wire_tmp1 = wire_cur1 / 2;
    assign wire_tmp2 = wire_cur1 * 3;
    assign wire_tmp3 = reg_tmp2 + 1;
    assign wire_tmp4 = wire_cur1 % 2;
    assign wire_tmp5 = reg_tmp4 == 0;
    assign wire_cur2 = wire_tmp5 ? wire_tmp1 : wire_tmp3;
    assign wire_step2 = wire_step1 + 1;
    assign wire_test1 = wire_cur1 > 1;
    assign wire_while = test1;

    always @(posedge clk) begin
        if (st_loop_en) begin
            // II counter
            if (st_loop_cnt == 2'd1) begin
                st_loop_cnt <= 0;
            end else begin
                st_loop_cnt <= st_loop_cnt + 1;
            end

            // Data path
            if (st_loop_stage_en[0]) begin
                reg_cur1 <= wire_cur1;
                reg_tmp2 <= wire_tmp2;
                reg_tmp4 <= wire_tmp4;
                reg_step2 <= wire_step2;
                st_loop_while <= wire_while;
            end else if (st_loop_stage_en[1]) begin
                reg_cur2 <= wire_cur2;
            end

            // Stage enable signals & signal for mu
            if (st_loop_cnt == 2'b0) begin
                st_loop_stage_en[0] <= st_loop_while;
                st_loop_stage_en[1] <= st_loop_stage_en[0];
            end

            if (st_loop_cnt == 2'b0) begin
                if (st_loop_is_first) begin
                    st_loop_is_first <= 1'b0;
                end
                st_loop_stage_is_first[0] <= st_loop_is_first;
                st_loop_stage_is_first[1] <= st_loop_is_first[0];
            end
        end
    end

    //
    // ST_EXIT
    //
    reg [0:0] st_exit_cnt;

    reg [31:0] reg_step;
    wire [31:0] wire_step;

    assign wire_step = prev_state == ST_INIT ? reg_step0 : reg_step2;

    always @(posedge clk) begin
        if (st_exit_en) begin
            if (st_exit_cnt == 1'b0) begin
                // compute
                reg_step <= wire_step;
                // finish
                st_exit_fin <= 1;
            end
        end
    end

endmodule
