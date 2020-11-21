// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : Exp



module Exp (
  input      [3:0]    io_a_0,
  input      [3:0]    io_a_1,
  input      [3:0]    io_a_2,
  input      [3:0]    io_a_3,
  input      [3:0]    io_b_0,
  input      [3:0]    io_b_1,
  input      [3:0]    io_b_2,
  input      [3:0]    io_b_3,
  output     [3:0]    io_c_0,
  output     [3:0]    io_c_1,
  output     [3:0]    io_c_2,
  output     [3:0]    io_c_3
);

  assign io_c_0 = (io_a_0 ^ io_b_0);
  assign io_c_1 = (io_a_1 ^ io_b_1);
  assign io_c_2 = (io_a_2 ^ io_b_2);
  assign io_c_3 = (io_a_3 ^ io_b_3);

endmodule
