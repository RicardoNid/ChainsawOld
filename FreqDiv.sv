// frequency divider
module FreqDiv
	#(
		parameter factor = 5
	)
	(
		input clk_in,
		input rst, // sync, active-high reset
		output clk_out
	);

	// declarations
	logic clk_p, clk_n;
	typedef logic [$clog2(factor)-1:0] counter_t;
	counter_t risingCounter, fallingCounter, risingCounterNext, fallingCounterNext;

	always_comb begin
		if(risingCounter == factor - 1)begin
			risingCounterNext = 0;
		end
		else begin
			risingCounterNext = risingCounter + 1;
		end
		if(fallingCounter == factor - 1)begin
			fallingCounterNext = 0;
		end
		else begin
			fallingCounterNext = fallingCounter + 1;
		end
	end

	always_ff @ (posedge clk_in) begin
		if (rst) begin
			risingCounter <= 0;
		end
		else begin
			risingCounter <= risingCounterNext;
		end
	end

	always_ff @ (negedge clk_in) begin
		if(rst) begin
			fallingCounter <= 0;
		end
		else begin
			fallingCounter <= fallingCounterNext;
		end
	end


    assign clk_p = risingCounter < (factor / 2);
    assign clk_n = fallingCounter < (factor / 2);
	assign clk_out = clk_p || clk_n;

endmodule
