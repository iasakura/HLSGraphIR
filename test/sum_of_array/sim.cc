#include <cassert>
#include <iostream>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vtop.h"

int sum_of_array_ref(const std::vector<int32_t> &vec) {
    int32_t sum = 0;
    for (int i = 0; i < vec.size(); ++i) {
        sum += vec[i];
    }
    return sum;
}

template<typename T>
void print_array(size_t n, T *arr) {
    for (int i = 0; i < n; ++i) {
        std::cout << arr[i] << ", ";
    }
    std::cout << "\n";
}

void test(int n, const std::string& name) {
    int time_counter = 0;

    // Instantiate DUT
    Vtop *dut = new Vtop();

    Verilated::traceEverOn(true);
    VerilatedVcdC* tfp = new VerilatedVcdC;
    dut->trace(tfp, 100);  // Trace 100 levels of hierarchy
    const std::string filename = name + "_trace.vcd";
    tfp->open(filename.c_str());

    std::vector<int32_t> vec;
    for (int i = 0; i < n; ++i) {
        vec.push_back(i);
    }

    for (int i = 0; i < n; ++i) {
        dut->top__DOT__m0__DOT__ram[i] = vec[i];
    }

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
            printf("read_addr = %d\n", dut->top__DOT__arr_read_val);
            print_array(n, dut->top__DOT__m0__DOT__ram);
        }
        dut->eval();
        tfp->dump(time_counter);
        finish = dut->finish;
    }

    printf("sum_of_array(%d) = %d in %d cycle\n", n, dut->res, cycle);

    dut->final();
    tfp->close();

    assert(sum_of_array_ref(vec) == dut->res);
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    test(1, "sum_of_array_1");
    test(2, "sum_of_array_2");
    test(10, "sum_of_array_10");
    test(20, "sum_of_array_20");
    test(30, "sum_of_array_30");

    std::cout << "Test passed!" << std::endl;
}
