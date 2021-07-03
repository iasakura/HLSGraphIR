#include <cassert>
#include <iostream>

#include <verilated.h>
#include "Vsimple.h"

int simple_ref(int n) {
    return n + 1;
}

void test(int n) {
    int time_counter = 0;

    // Instantiate DUT
    Vsimple *dut = new Vsimple();

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

    dut->inp = n;
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
        finish = dut->finish;
    }

    printf("Final Counter Value = %d in %d cycle\n", dut->res, cycle);
    assert(simple_ref(n) == dut->res);

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
