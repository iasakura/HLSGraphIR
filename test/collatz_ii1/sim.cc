#include <cassert>
#include <iostream>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vcollatz_ii1.h"

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

void test(int n, const std::string &name) {
    int time_counter = 0;

    // Instantiate DUT
    Vcollatz_ii1 *dut = new Vcollatz_ii1();

    Verilated::traceEverOn(true);
    VerilatedVcdC* tfp = new VerilatedVcdC;
    dut->trace(tfp, 100);  // Trace 100 levels of hierarchy
    const std::string filename = name + "_trace.vcd";
    tfp->open(filename.c_str());

    // Format
    dut->rst_n = 0;
    dut->clk = 0;

    // Reset Time
    while (time_counter < 100) {
        dut->clk = !dut->clk;
        dut->eval();
        time_counter++;
        tfp->dump(time_counter);
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
            printf("state = %02x ", dut->collatz_ii1__DOT__cur_state);
            printf("init0_en = %02x ", dut->collatz_ii1__DOT__INIT0_en);
            printf("init0_done = %02x ", dut->collatz_ii1__DOT__INIT0_done);
            printf("init1_en = %02x ", dut->collatz_ii1__DOT__INIT1_en);
            printf("init1_done = %02x ", dut->collatz_ii1__DOT__INIT1_done);
            printf("loop_en = %02x ", dut->collatz_ii1__DOT__LOOP_en);
            printf("loop_done = %02x ", dut->collatz_ii1__DOT__LOOP_done);
            // // printf("init_done = %02x ", dut->collatz__DOT__st_init_done);
            printf("en[] = %02x, %02x, %02x ", dut->collatz_ii1__DOT__LOOP_stage_en[0], dut->collatz_ii1__DOT__LOOP_stage_en[1], dut->collatz_ii1__DOT__LOOP_stage_en[2]);
            // printf("en1 = %02x ", dut->collatz__DOT__LOOP_stage_en[1]);
            // printf("cond = %02x ", dut->collatz__DOT__LOOP_loop_cond);
            printf("is_first = %02x ", dut->collatz_ii1__DOT__LOOP_is_first);
            printf("is_first[] = %02x, %02x, %02x ", dut->collatz_ii1__DOT__LOOP_stage_is_first[0], dut->collatz_ii1__DOT__LOOP_stage_is_first[1], dut->collatz_ii1__DOT__LOOP_stage_is_first[2]);
            // printf("cnt = %02x ", dut->collatz__DOT__LOOP_cnt);
            printf("cur0 = %02x ", dut->collatz_ii1__DOT__cur0);
            printf("loop_cur0 = %02x ", dut->collatz_ii1__DOT__loop_cur0);
            printf("loop_cur1 = %02x ", dut->collatz_ii1__DOT__loop_cur1);
            printf("init_cur1 = %02x ", dut->collatz_ii1__DOT__init_cur1);
            printf("step = %02x ", dut->collatz_ii1__DOT__step);
            printf("res = %02x ", dut->res);
            printf("\n");
        }
        dut->eval();
        tfp->dump(time_counter);
        finish = dut->finish;
    }

    printf("Final Counter Value = %d in %d cycle\n", dut->res, cycle);

    dut->final();
    tfp->close();

    assert(collatz_ref(n) == dut->res);
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    test(1, "collatz1");
    test(2, "collatz2");
    test(10, "collatz10");
    test(20, "collatz20");
    test(30, "collatz30");

    std::cout << "Test passed!" << std::endl;
}
