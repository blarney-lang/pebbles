// Simulation wrapper for pipelined divider
module IntelDivider (
		input  wire [32:0] numer,
		input  wire [32:0] denom,
		input  wire        clock,
		output wire [32:0] quotient,
		output wire [32:0] remain
	);

  parameter LATENCY = 12;
  reg [32:0] quotient_q[LATENCY-1:0];
  reg [32:0] remain_q[LATENCY-1:0];

  generate
    genvar i;
    for (i = 0; i < LATENCY-1; i=i+1) begin
      always @(posedge clock) begin
        quotient_q[i] <= quotient_q[i+1];
        remain_q[i] <= remain_q[i+1];
      end
    end
  endgenerate

  always @(posedge clock) begin
    if (denom == 0) begin
      quotient_q[LATENCY-1] <= -1;
      remain_q[LATENCY-1] <= numer;
    end else if (numer == -(2**31) && denom == -1) begin
      quotient_q[LATENCY-1] <= numer;
      remain_q[LATENCY-1] <= 0;
    end else begin
      quotient_q[LATENCY-1] <= $signed(numer) / $signed(denom);
      remain_q[LATENCY-1] <= $signed(numer) % $signed(denom);
    end
  end

  assign quotient = quotient_q[0];
  assign remain = remain_q[0];

endmodule
