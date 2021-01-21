#include <iostream>
#include <verilated.h>
#include "Vcollatz.h"

void test(int n) {
    int time_counter = 0;

    // Instantiate DUT
    Vcollatz *dut = new Vcollatz();

    // Format
    dut->rst_n = 0;
    dut->clk = 0;

    // Reset Time
    while (time_counter < 100) {
        dut->clk = !dut->clk;
        dut->eval();
        time_counter++;
    }
    // Release reset
    dut->rst_n = 1;

    dut->n = n;
    dut->start = 1;

    int cycle = 0;
    bool finish = false;
    while (!finish) {
        dut->clk = !dut->clk;
        time_counter++;
        if (time_counter % 2 == 0) {
            cycle++;
        }

        dut->eval();
        // if (dut->clk) {
        //     printf("state = %02x ", dut->collatz__DOT__cur_state);
        //     printf("loop_en = %02x ", dut->collatz__DOT__st_loop_en);
        //     // printf("init_done = %02x ", dut->collatz__DOT__st_init_done);
        //     printf("en0 = %02x ", dut->collatz__DOT__st_loop_stage_en[0]);
        //     printf("en1 = %02x ", dut->collatz__DOT__st_loop_stage_en[1]);
        //     printf("flush = %02x ", dut->collatz__DOT__st_loop_pipeline_flush);
        //     printf("cnt = %02x ", dut->collatz__DOT__st_loop_cnt);
        //     printf("cur1 = %02x ", dut->collatz__DOT__wire_cur1);
        //     printf("step2 = %02x ", dut->collatz__DOT__reg_step2);
        //     printf("\n");
        // }
        finish = dut->finish;
    }

    printf("Final Counter Value = %d in %d cycle\n", dut->ret0, cycle);

    dut->final();
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    test(1);
    test(10);
    test(20);
    test(30);
}
