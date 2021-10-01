// Generator : SpinalHDL v1.5.0    git head : 83a031922866b078c411ec5529e00f1b6e79f8e7
// Component : unamed
// Git hash  : ebb5c247e210fb308ecbf93fcefd8a85ef73d266



module unamed (
  input               clk,
  input               reset
);
  wire       [3:0]    _zz_dataOut_1;
  wire       [3:0]    dataIn;
  wire       [3:0]    dataOut;
  reg        [3:0]    _zz_dataOut;

  assign _zz_dataOut_1 = (_zz_dataOut + 4'b0001);
  assign dataOut = _zz_dataOut_1;
  always @(posedge clk) begin
    _zz_dataOut <= dataIn;
  end


endmodule
