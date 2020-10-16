#include <verilated.h>
#include "VSimPebbles.h"

VSimPebbles *top;
vluint64_t main_time = 0;

// Called by $time in Verilog
double sc_time_stamp () {
  return main_time;
}

int main(int argc, char** argv) {
  Verilated::commandArgs(argc, argv);
  top = new VSimPebbles;
  while (!Verilated::gotFinish()) {
    top->clock = 0; top->eval();
    top->clock = 1; top->eval();
    main_time++;
  }
  top->final();
  delete top;
  return 0;
}
