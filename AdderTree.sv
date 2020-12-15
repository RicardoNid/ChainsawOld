// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : AdderTree
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module AdderTree (
  input      [7:0]    io_operands_0,
  input      [7:0]    io_operands_1,
  input      [7:0]    io_operands_2,
  input      [7:0]    io_operands_3,
  input      [7:0]    io_operands_4,
  input      [7:0]    io_operands_5,
  (* dont_touch = "yes" *) input      [7:0]    io_operands_6,
  input      [7:0]    io_operands_7,
  input      [7:0]    io_operands_8,
  input      [7:0]    io_operands_9,
  input      [7:0]    io_operands_10,
  input      [7:0]    io_operands_11,
  input      [7:0]    io_operands_12,
  output     [11:0]   io_result,
  input               clk,
  input               reset
);
  wire       [8:0]    _zz_13;
  wire       [9:0]    _zz_14;
  wire       [9:0]    _zz_15;
  wire       [10:0]   _zz_16;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_1;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_2;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_3;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_4;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_5;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_6;
  reg        [8:0]    _zz_1_regNext;
  reg        [8:0]    _zz_2_regNext;
  reg        [8:0]    _zz_3_regNext;
  (* dont_touch = "yes" *) reg        [8:0]    _zz_4_regNext;
  reg        [8:0]    _zz_5_regNext;
  reg        [8:0]    _zz_6_regNext;
  reg        [7:0]    io_operands_6_regNext;
  (* dont_touch = "yes" *) wire       [9:0]    _zz_7;
  (* dont_touch = "yes" *) wire       [9:0]    _zz_8;
  (* dont_touch = "yes" *) wire       [9:0]    _zz_9;
  reg        [9:0]    _zz_7_regNext;
  reg        [9:0]    _zz_8_regNext;
  reg        [9:0]    _zz_9_regNext;
  reg        [8:0]    _zz_4_regNext_regNext;
  (* dont_touch = "yes" *) wire       [10:0]   _zz_10;
  (* dont_touch = "yes" *) wire       [10:0]   _zz_11;
  reg        [10:0]   _zz_10_regNext;
  reg        [10:0]   _zz_11_regNext;
  (* dont_touch = "yes" *) wire       [11:0]   _zz_12;
  reg        [11:0]   _zz_12_regNext;

  assign _zz_13 = {1'b0,io_operands_6_regNext};
  assign _zz_14 = {1'd0, _zz_13};
  assign _zz_15 = {1'b0,_zz_4_regNext_regNext};
  assign _zz_16 = {1'd0, _zz_15};
  assign _zz_1 = ({1'b0,io_operands_0} + {1'b0,io_operands_7});
  assign _zz_2 = ({1'b0,io_operands_1} + {1'b0,io_operands_8});
  assign _zz_3 = ({1'b0,io_operands_2} + {1'b0,io_operands_9});
  assign _zz_4 = ({1'b0,io_operands_3} + {1'b0,io_operands_10});
  assign _zz_5 = ({1'b0,io_operands_4} + {1'b0,io_operands_11});
  assign _zz_6 = ({1'b0,io_operands_5} + {1'b0,io_operands_12});
  assign _zz_7 = ({1'b0,_zz_1_regNext} + {1'b0,_zz_5_regNext});
  assign _zz_8 = ({1'b0,_zz_2_regNext} + {1'b0,_zz_6_regNext});
  assign _zz_9 = ({1'b0,_zz_3_regNext} + _zz_14);
  assign _zz_10 = ({1'b0,_zz_7_regNext} + {1'b0,_zz_9_regNext});
  assign _zz_11 = ({1'b0,_zz_8_regNext} + _zz_16);
  assign _zz_12 = ({1'b0,_zz_10_regNext} + {1'b0,_zz_11_regNext});
  assign io_result = _zz_12_regNext;
  always @ (posedge clk) begin
    _zz_1_regNext <= _zz_1;
    _zz_2_regNext <= _zz_2;
    _zz_3_regNext <= _zz_3;
    _zz_4_regNext <= _zz_4;
    _zz_5_regNext <= _zz_5;
    _zz_6_regNext <= _zz_6;
    io_operands_6_regNext <= io_operands_6;
    _zz_7_regNext <= _zz_7;
    _zz_8_regNext <= _zz_8;
    _zz_9_regNext <= _zz_9;
    _zz_4_regNext_regNext <= _zz_4_regNext;
    _zz_10_regNext <= _zz_10;
    _zz_11_regNext <= _zz_11;
    _zz_12_regNext <= _zz_12;
  end


endmodule
