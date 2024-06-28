import "DPI-C" function int c_FPMul(input int a, input int b);

// Latency: 4 cycles
module FPHardMul (
		input  wire        clk0, 
		input  wire        ena,
		input  wire        clr0,
		input  wire [31:0] ay,
		input  wire [31:0] az,
		output wire [31:0] result
	);

  parameter LATENCY = 4;
  reg [31:0] result_reg[LATENCY-1:0];

  generate
    genvar i;
    for (i = 0; i < LATENCY-1; i=i+1) begin
      always @(posedge clk0) begin
        result_reg[i] <= result_reg[i+1];
      end
    end
  endgenerate

  always @(posedge clk0) begin
    result_reg[LATENCY-1] <= c_FPMul(ay, az);
  end

  assign result = result_reg[0];

endmodule
