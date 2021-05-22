// Generator : SpinalHDL v1.4.3    git head : adf552d8f500e7419fff395b7049228e4bc5de26
// Component : SCMMine
// Git hash  : 12bfb984652f2611a439df488b084f39e305c22b



module SCMMine (
  input      [14:0]   input_1,
  output reg [21:0]   output_1,
  input               clk,
  input               reset
);
  wire       [15:0]   _zz_6;
  wire       [13:0]   _zz_7;
  wire       [15:0]   _zz_8;
  wire       [15:0]   _zz_9;
  wire       [1:0]    _zz_10;
  wire       [17:0]   _zz_11;
  wire       [13:0]   _zz_12;
  wire       [17:0]   _zz_13;
  wire       [3:0]    _zz_14;
  wire       [14:0]   _zz_1;
  wire       [14:0]   _zz_2;
  wire       [12:0]   _zz_3;
  wire       [17:0]   _zz_4;
  wire       [17:0]   _zz_5;

  assign _zz_1 = input_1;

  assign _zz_2 = input_1;
  assign _zz_3 = _zz_1[14 : 2];
  assign _zz_10 = _zz_1[1 : 0];
  assign _zz_8 = {{2{_zz_7[13]}}, _zz_7};
  assign _zz_6 = ($signed(_zz_8) + $signed(_zz_9));
  assign _zz_4 = {_zz_6,_zz_10};

  assign _zz_12 = _zz_5[17 : 4];
  assign _zz_13 = {{4{_zz_12[13]}}, _zz_12};
  assign _zz_11 = ($signed(_zz_13) + $signed(_zz_4));

  assign _zz_5 = _zz_4;
  assign _zz_14 = _zz_5[3 : 0];

  assign _zz_7 = {_zz_3[12],_zz_3};
  assign _zz_9 = {_zz_2[14],_zz_2};
  always @ (posedge clk) begin
    output_1 <= {_zz_11,_zz_14};
  end


endmodule