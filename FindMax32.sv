//
module FindMax32
	#(
		parameter integer  PIPELINED = 0
	)
	(
//		input logic [31:0] data_in [0:31],
		input logic [1023:0] data_in,
		output logic [31:0] data_out
	);

	logic [31:0] data_in_vec [0:31];

	always_comb begin

		logic [31:0] temp [0:31];

//		temp = data_in;

		for (int i=0; i<32; i++) begin
			automatic integer high = 1023 - i * 32;
			for (int j=0; j<32; j++) begin
				data_in_vec[i][31 - j] = data_in[high - j];
			end
		end

		temp = data_in_vec;

		for (int i=4; i>=0; i--) begin
			for (int j=0; j<1<<i; j++) begin
				integer low = j;
				integer high = j + (1 << i);
				temp[low] = temp[low] > temp[high] ? temp[low] : temp[high];
			end
		end
		data_out = temp[0];
	end

endmodule
