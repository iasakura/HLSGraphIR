#include <cassert>
#include <iostream>

#include <verilated.h>
#include "Vcollatz.h"

int collatz_ref(int n) {
    int cur = n;
    int step = 0;
    while (cur != 1) {
        step++;
        if (cur % 2 == 0) {
            cur = cur / 2;
        } else {
            cur = cur * 3 + 1;
        }
    }
    return step;
}

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

        if (dut->clk) {
            printf("state = %02x ", dut->collatz__DOT__cur_state);
            printf("init_en = %02x ", dut->collatz__DOT__INIT_en);
            printf("init_done = %02x ", dut->collatz__DOT__INIT_done);
            printf("loop_en = %02x ", dut->collatz__DOT__LOOP_en);
            printf("loop_done = %02x ", dut->collatz__DOT__LOOP_done);
            // printf("init_done = %02x ", dut->collatz__DOT__st_init_done);
            printf("en0 = %02x ", dut->collatz__DOT__LOOP_stage_en[0]);
            printf("en1 = %02x ", dut->collatz__DOT__LOOP_stage_en[1]);
            printf("cond = %02x ", dut->collatz__DOT__LOOP_loop_cond);
            printf("is_first = %02x ", dut->collatz__DOT__LOOP_is_first);
            printf("cnt = %02x ", dut->collatz__DOT__LOOP_cnt);
            printf("cur1 = %02x ", dut->collatz__DOT__cur2);
            printf("step2 = %02x ", dut->collatz__DOT__step2);
            printf("\n");
        }
        dut->eval();
        finish = dut->finish;
    }

    printf("Final Counter Value = %d in %d cycle\n", dut->step, cycle);
    assert(collatz_ref(n) == dut->step);

    dut->final();
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    test(1);
    test(2);
    test(10);
    test(20);
    test(30);

    std::cout << "Test passed!" << std::endl;
}
