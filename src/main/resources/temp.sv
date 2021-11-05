// example

typedef struct packed{
	logic alu_md, alu_cl;
} alu_func;

module temp (
	input logic clk,
    input alu_func dataIn,
    output alu_func dataOut);

	always_ff @ (posedge  clk) begin
		dataOut.alu_md <= dataIn.alu_cl ;
		dataOut.alu_cl <= dataIn.alu_md;
	end

endmodule
