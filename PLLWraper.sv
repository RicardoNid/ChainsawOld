// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : PLLWraper
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815



module PLLWraper (
  input               io_aReset,
  input               io_clk100MhzP,
  input               io_clk100MhzN,
  output     [3:0]    io_result
);
  wire                _zz_2;
  wire                clkCtrl_pll_clk_out1;
  wire                clkCtrl_pll_locked;
  wire                bufferCC_1_io_dataOut;
  wire                core_clk;
  wire                core_reset;
  wire                _zz_1;
  reg        [3:0]    core_counter;

  singleClock clkCtrl_pll (
    .clk_in1_p    (io_clk100MhzP         ), //i
    .clk_in1_n    (io_clk100MhzN         ), //i
    .clk_out1     (clkCtrl_pll_clk_out1  ), //o
    .locked       (clkCtrl_pll_locked    )  //o
  );
  BufferCC bufferCC_1 (
    .io_dataIn     (_zz_2                  ), //i
    .io_dataOut    (bufferCC_1_io_dataOut  ), //o
    .core_clk      (core_clk               ), //i
    ._zz_1         (_zz_1                  )  //i
  );
  assign core_clk = clkCtrl_pll_clk_out1;
  assign _zz_1 = (io_aReset || (! clkCtrl_pll_locked));
  assign _zz_2 = 1'b0;
  assign core_reset = bufferCC_1_io_dataOut;
  assign io_result = core_counter;
  always @ (posedge core_clk or posedge core_reset) begin
    if (core_reset) begin
      core_counter <= 4'b0000;
    end else begin
      core_counter <= (core_counter + 4'b0001);
    end
  end


endmodule

module BufferCC (
  input               io_dataIn,
  output              io_dataOut,
  input               core_clk,
  input               _zz_1
);
  reg                 buffers_0;
  reg                 buffers_1;

  assign io_dataOut = buffers_1;
  always @ (posedge core_clk or posedge _zz_1) begin
    if (_zz_1) begin
      buffers_0 <= 1'b1;
      buffers_1 <= 1'b1;
    end else begin
      buffers_0 <= io_dataIn;
      buffers_1 <= buffers_0;
    end
  end


endmodule
