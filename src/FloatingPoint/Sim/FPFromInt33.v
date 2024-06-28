import "DPI-C" function int c_FPFromInt64(input longint a);

// Latency: 7 cycles
module FPFromInt33 (
		input  wire        clk,    //    clk.clk
		input  wire        areset, // areset.reset
		input  wire [32:0] a,      //      a.a
		output wire [31:0] q       //      q.q
	);

  parameter LATENCY = 7;
  reg [31:0] result[LATENCY-1:0];

  generate
    genvar i;
    for (i = 0; i < LATENCY-1; i=i+1) begin
      always @(posedge clk) begin
        result[i] <= result[i+1];
      end
    end
  endgenerate

  always @(posedge clk) begin
    result[LATENCY-1] <= c_FPFromInt64({ {32{a[32]}}, a[31:0] });
  end

  assign q = result[0];

endmodule
