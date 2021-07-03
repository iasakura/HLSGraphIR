#include <cassert>
#include <iostream>

#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vtop.h"

std::vector<int32_t> stream_add2_ref(const std::vector<int32_t> &vec1, const std::vector<int32_t>& vec2) {
    std::vector<int32_t> ret;
    for (int i = 0; i < vec1.size(); ++i) {
        ret.push_back(vec1[i] + vec2[i]);
    }
    return ret;
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

    std::vector<int32_t> vec1, vec2;
    for (int i = 0; i < n; ++i) {
        vec1.push_back(i);
        vec2.push_back(i * 2);
    }

    for (int i = 0; i < n; ++i) {
        dut->top__DOT__stream_src1__DOT__mem[i] = vec1[i];
        dut->top__DOT__stream_src2__DOT__mem[i] = vec2[i];
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
            // printf("read_addr = %d\n", dut->top__DOT__arr_read_val);
            // print_array(n, dut->top__DOT__m0__DOT__ram);
        }
        dut->eval();
        tfp->dump(time_counter);
        finish = dut->finish;
        // printf("finish: %d\n", finish);
    }

    std::vector<int32_t> res;
    for (int i = 0; i < n; ++i) {
        res.push_back((int32_t)(dut->top__DOT__stream_sink__DOT__mem[i]));
    }

    // printf("stream_add2(%d) = %d in %d cycle\n", n, dut->res, cycle);

    dut->final();
    tfp->close();

    const auto expected = stream_add2_ref(vec1, vec2);
    std::cout << "expected: ";
    for (auto v : expected) {
        std::cout << v << ", ";
    }
    std::cout << "\n";
    std::cout << "actual: ";
    for (auto v : res) {
        std::cout << v << ", ";
    }
    std::cout << "\n";
    assert(expected == res);
    std::cout << "Test " << name << " passed!" << std::endl;
}

int main(int argc, char** argv) {
    Verilated::commandArgs(argc, argv);

    test(1, "stream_add2_1");
    test(2, "stream_add2_2");
    test(3, "stream_add2_3");
    test(4, "stream_add2_4");
    test(10, "stream_add2_10");
    test(20, "stream_add2_20");
    test(30, "stream_add2_30");

    std::cout << "Test passed!" << std::endl;
}
