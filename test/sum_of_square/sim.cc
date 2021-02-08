#include <cassert>
#include <iostream>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vsum_of_square.h"

int sum_of_square_ref(int n) {
    int sum = 0;
    for (int i = 1; i <= n; ++i) {
        sum += i * i;
    }
    return sum;
}

void test(int n, const std::string& name) {
    int time_counter = 0;

    // Instantiate DUT
    Vsum_of_square *dut = new Vsum_of_square();

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
        }
        dut->eval();
        tfp->dump(time_counter);
        finish = dut->finish;
    }

    printf("sum_of_square(%d) = %d in %d cycle\n", n, dut->res, cycle);

    dut->final();
    tfp->close();

    assert(sum_of_square_ref(n) == dut->res);
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    test(1, "sum_of_square1");
    test(2, "sum_of_square2");
    test(10, "sum_of_square10");
    test(20, "sum_of_square20");
    test(30, "sum_of_square30");

    std::cout << "Test passed!" << std::endl;
}
