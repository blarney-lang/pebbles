module FPHardMulWrapper (
    input  wire        clock,
    input  wire        reset,
    input  wire        ena,
    input  wire [31:0] ay,
    input  wire [31:0] az,
    output wire [31:0] result
  );

  FPHardMul FPHardMulInst (
    .clk0(clock)
  , .ena(ena)
  , .clr0(reset)
  , .ay(ay)
  , .az(az)
  , .result(result)
  );
endmodule
