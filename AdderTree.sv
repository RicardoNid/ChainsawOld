// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : AdderTree
// Git hash  : 26498da3e635f410d1b36d95703a773e107a807d



module AdderTree (
  input      [7:0]    io_operands_0,
  input      [7:0]    io_operands_1,
  input      [7:0]    io_operands_2,
  (* dont_touch = "yes" *) input      [7:0]    io_operands_3,
  input      [7:0]    io_operands_4,
  input      [7:0]    io_operands_5,
  input      [7:0]    io_operands_6,
  output     [10:0]   io_result
);
  wire       [8:0]    _zz_7;
  wire       [9:0]    _zz_8;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_1;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_2;
  (* dont_touch = "yes" *) wire       [8:0]    _zz_3;
  (* dont_touch = "yes" *) wire       [9:0]    _zz_4;
  (* dont_touch = "yes" *) wire       [9:0]    _zz_5;
  (* dont_touch = "yes" *) wire       [10:0]   _zz_6;

  assign _zz_7 = {1'b0,io_operands_3};
  assign _zz_8 = {1'd0, _zz_7};
  assign _zz_1 = ({1'b0,io_operands_0} + {1'b0,io_operands_4});
  assign _zz_2 = ({1'b0,io_operands_1} + {1'b0,io_operands_5});
  assign _zz_3 = ({1'b0,io_operands_2} + {1'b0,io_operands_6});
  assign _zz_4 = ({1'b0,_zz_1} + {1'b0,_zz_3});
  assign _zz_5 = ({1'b0,_zz_2} + _zz_8);
  assign _zz_6 = ({1'b0,_zz_4} + {1'b0,_zz_5});
  assign io_result = _zz_6;

endmodule
