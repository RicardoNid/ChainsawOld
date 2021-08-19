// word-serial adder
localparam wordSize = 8;
typedef logic [wordSize - 1:0] wordType;

module Adder256
	(
	 input           clk,
	 input           rst,
	 input           valid_in,
	 input           last_in,
	 input  wordType data_in_0 ,
	 input  wordType data_in_1 ,
	 output logic    valid_out,
	 output logic    last_out,
	 output wordType data_out
	 );

	logic [wordSize:0] partial;

	always_ff @ (posedge clk) begin
		if (rst) begin
			valid_out         <= 0;
			last_out          <= 0;
			partial[wordSize] <= 0;
		end
		else begin
			if(last_in) begin // clear the carry after last word
				partial[wordSize-1:0] <= data_in_0 + data_in_1 + partial[wordSize];
				partial[wordSize]     <= 0;
			end
			else begin
				partial               <= data_in_0 + data_in_1 + partial[wordSize];
			end
			// the control signals are just delayed
			valid_out         <= valid_in;
			last_out          <= last_in;
		end
	end

	assign data_out = partial[wordSize - 1:0];


endmodule
