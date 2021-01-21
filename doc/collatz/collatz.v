`define to_int(val) \
    {{31{1'b0}}, (val)}
`define to_bool(val) \
    (val != 0)

module collatz(
    clk, rst_n,
    start, finish,
    n,
    ret0
);
    input clk;
    input rst_n;
    input start;
    output reg finish;
    input [31:0] n;
    output reg [31:0] ret0;

    parameter ST_INIT = 2'd0;
    parameter ST_LOOP = 2'd1;
    parameter ST_EXIT = 2'd2;
    parameter ST_FIN  = 2'd3;

    reg [1:0] cur_state, prev_state;
    reg st_init_done, st_loop_done, st_exit_done;
    reg st_init_en, st_loop_en, st_exit_en, st_fin_en;

    // State machine for CFG
    always @(posedge clk) begin
        if (!rst_n) begin
            prev_state <= ST_INIT; // DUMMY
            cur_state <= ST_INIT;
            // INITIALIZATION of INIT
            st_init_cnt <= 1'b0;
            // Enable INIT
            st_init_en <= 1'b1;
        end else begin
            if (cur_state == ST_INIT) begin
                if (st_init_done) begin
                    st_init_en <= 1'b0;
                    prev_state <= cur_state;
                    if (`to_bool(reg_test0)) begin
                        cur_state <= ST_LOOP;    
                        // INITIALIZATION of ST_LOOP  
                        st_loop_cnt <= 2'd0;
                        st_loop_stage_en[0] <= 1'b0;
                        st_loop_stage_en[1] <= 1'b0;
                        st_loop_pipeline_flush <= 1'b0;
                        st_loop_is_first <= 1'b1;
                        // Enable LOOP
                        st_loop_en <= 1'b1;
                    end else begin
                        cur_state <= ST_EXIT;
                        // INITIALIZATION of ST_EXIT
                        st_exit_cnt <= 1'b0;
                        // Enable EXIT
                        st_exit_en <= 1'b1;
                    end
                end
            end else if (cur_state == ST_LOOP) begin
                if (st_loop_done) begin
                    st_loop_en <= 1'b0;
                    prev_state <= cur_state;
                    cur_state <= ST_EXIT;
                    // INITIALIZATION of ST_EXIT
                    st_exit_cnt <= 1'b0;
                    // Enable EXIT
                    st_exit_en <= 1'b1;
                end
            end else if (cur_state == ST_EXIT) begin
                if (st_exit_done) begin
                    prev_state <= cur_state;
                    cur_state <= ST_FIN;
                    // FINALIZATION
                    st_fin_en <= 1'b1;
                end
            end
        end
    end

    reg [31:0] reg_cur0;
    reg [31:0] reg_step0;
    reg [31:0] reg_test0;

    wire [31:0] wire_cur0;
    wire [31:0] wire_step0;
    wire [31:0] wire_test0;
    
    assign wire_cur0 = n;
    assign wire_step0 = 32'b0;
    assign wire_test0 = `to_int(n > 32'b1);

    //
    // ST_INIT
    //
    // state in ST_INIT
    reg [0:0] st_init_cnt;

    always @(posedge clk) begin
        if (!rst_n) begin
            reg_cur0 <= 32'b0;
            reg_step0 <= 32'b0;
            reg_test0 <= 32'b0;
        end else if (st_init_en) begin
            if (st_init_cnt == 1'b0) begin
                // compute
                reg_cur0 <= wire_cur0;
                reg_step0 <= wire_step0;
                reg_test0 <= wire_test0;
                // done
                st_init_done <= 1;
            end
        end
    end

    //
    // ST_LOOP
    //
    // Pipeline states in ST_LOOP
    reg [1:0] st_loop_cnt;
    reg st_loop_stage_en[0:1];
    reg st_loop_is_first;
    reg st_loop_stage_is_first[0:1];
    reg st_loop_pipeline_flush;

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
    wire st_loop_wire_while;

    wire st_loop_stage_en_all_disabled = &{!st_loop_stage_en[0], !st_loop_stage_en[1]};

    assign wire_cur1 = st_loop_stage_is_first[0] ? reg_cur0 : reg_cur2;
    assign wire_step1 = st_loop_stage_is_first[0] ? reg_step0 : reg_step2;
    assign wire_tmp1 = wire_cur1 / 2;
    assign wire_tmp2 = wire_cur1 * 3;
    assign wire_tmp3 = reg_tmp2 + 1;
    assign wire_tmp4 = wire_cur1 % 2;
    assign wire_tmp5 = `to_int(reg_tmp4 == 32'b0);
    assign wire_cur2 = `to_bool(wire_tmp5) ? wire_tmp1 : wire_tmp3;
    assign wire_step2 = wire_step1 + 1;
    assign wire_test1 = `to_int(wire_cur1 > 32'b1);
    assign st_loop_wire_while = `to_bool(wire_test1);

    always @(posedge clk) begin
        if (!rst_n) begin
            st_loop_cnt <= 2'b0;
            st_loop_stage_en[0] <= 1'b0;
            st_loop_stage_en[1] <= 1'b0;
            st_loop_is_first <= 1'b0;
            st_loop_stage_is_first[0] <= 1'b0;
            st_loop_stage_is_first[1] <= 1'b0;
            st_loop_pipeline_flush <= 1'b0;

            reg_cur1 <= 32'b0;
            reg_tmp2 <= 32'b0;
            reg_tmp4 <= 32'b0;
            reg_cur2 <= 32'b0;
            reg_step2 <= 32'b0;
        end else if (st_loop_en) begin
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
            end else if (st_loop_stage_en[1]) begin
                reg_cur2 <= wire_cur2;
            end

            // Go to flush phase
            if (st_loop_stage_en[0]) begin
                if (!st_loop_pipeline_flush) begin
                    st_loop_pipeline_flush <= !st_loop_wire_while;
                end
            end

            // Stage enable signals
            st_loop_stage_en[0] <= (st_loop_is_first || st_loop_wire_while) && !st_loop_pipeline_flush && (st_loop_cnt == 0);
            st_loop_stage_en[1] <= st_loop_stage_en[0];

            // is_first for mu
            if (st_loop_cnt == 2'b0) begin
                if (st_loop_is_first) begin
                    st_loop_is_first <= 1'b0;
                end
                st_loop_stage_is_first[0] <= st_loop_is_first;
                st_loop_stage_is_first[1] <= st_loop_stage_is_first[0];
            end

            // done signal
            st_loop_done <= !st_loop_is_first & st_loop_stage_en_all_disabled & st_loop_pipeline_flush;
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
        if (!rst_n) begin
            st_exit_cnt <= 0;
            reg_step <= 0;
        end if (st_exit_en) begin
            if (st_exit_cnt == 1'b0) begin
                // compute
                reg_step <= wire_step;
                // done
                st_exit_done <= 1;
            end
        end
    end

    //
    // FINISH
    //
    always @(posedge clk) begin
        if (st_fin_en) begin
            finish <= 1'b1;
            ret0 <= reg_step;
        end
    end

endmodule
