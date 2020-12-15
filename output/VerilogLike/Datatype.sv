// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : Datatype
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module Datatype (
  input               io_input0,
  input      [9:0]    io_input1,
  input      [9:0]    io_input2,
  input      [9:0]    io_input3,
  output              io_output0,
  output     [9:0]    io_output1,
  output     [9:0]    io_output2,
  output     [9:0]    io_output3
);
  wire       [9:0]    _zz_1;
  wire       [9:0]    _zz_2;

  assign _zz_1 = (io_input1 + io_input2);
  assign _zz_2 = io_input2;
  assign io_output0 = (! io_input0);
  assign io_output1 = _zz_1;
  assign io_output2 = (io_input2 + io_input3);
  assign io_output3 = ($signed(io_input3) + $signed(_zz_2));

endmodule
