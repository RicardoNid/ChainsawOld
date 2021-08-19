//
module FindMax32Pipelined
	(
		input clk, rst, valid_in,
		input logic [1023:0] data_in,
//      input logic [31:0] data_in [0:31],
		output logic [31:0] data_out,
		output logic valid_out
	);


	logic [31:0] data_in_vec [0:31];
	always_comb begin
		for (int i=0; i<32; i++) begin
			integer high = 1023 - i * 32;
			for (int j=0; j<32; j++) begin
				data_in_vec[i][31 - j] = data_in[high - j];
			end
		end;
	end

	logic [31:0] stage1 [0:15];
	logic [31:0] stage2 [0:7];
	logic [31:0] stage3 [0:3];
	logic [31:0] stage4 [0:1];
	logic valid_delays [0:3];

	always_ff @ (posedge clk) begin
		if (rst) begin
		    valid_out <= 0;
			valid_delays <= '{4{0}};
		end
		else begin
			// comparison logic
			for (int i=0; i<16; i++) begin
//          stage1[i] <= data_in[2*i] > data_in[2*i + 1] ? data_in[2*i] : data_in[2*i + 1];
				stage1[i] <= data_in_vec[2*i] > data_in_vec[2*i + 1] ? data_in_vec[2*i] : data_in_vec[2*i + 1];
			end
			for (int i=0; i<8; i++) begin
				stage2[i] <= stage1[2*i] > stage1[2*i + 1] ? stage1[2*i] : stage1[2*i + 1];
			end
			for (int i=0; i<4; i++) begin
				stage3[i] <= stage2[2*i] > stage2[2*i + 1] ? stage2[2*i] : stage2[2*i + 1];
			end
			for (int i=0; i<2; i++) begin
				stage4[i] <= stage3[2*i] > stage3[2*i + 1] ? stage3[2*i] : stage3[2*i + 1];
			end
			data_out <= stage4[0] > stage4[1] ? stage4[0] : stage4[1];

			// valid delay line
			valid_delays[0] <= valid_in;
			for (int i=0; i<3; i++) begin
				valid_delays[i+1] <= valid_delays[i];
			end
			valid_out <= valid_delays[3];
		end
	end

endmodule
