// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : WinogradDFT
// Git hash  : 0ddbc7657b05706f03c0a8a363d7dc0d9d001858



module WinogradDFT (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  input      [16:0]   io_input_4,
  input      [16:0]   io_input_5,
  input      [16:0]   io_input_6,
  input      [16:0]   io_input_7,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3,
  output     [16:0]   io_output_4,
  output     [16:0]   io_output_5,
  output     [16:0]   io_output_6,
  output     [16:0]   io_output_7
);
  wire       [16:0]   _zz_9;
  wire       [16:0]   _zz_10;
  wire       [16:0]   _zz_1;
  wire       [16:0]   _zz_2;
  wire       [16:0]   _zz_3;
  wire       [16:0]   _zz_4;
  wire       [16:0]   _zz_5;
  wire       [16:0]   _zz_6;
  wire       [16:0]   _zz_7;
  wire       [16:0]   _zz_8;

  assign _zz_9 = 17'h0;
  assign _zz_10 = ($signed(io_input_3) - $signed(io_input_7));
  assign _zz_1 = ($signed(io_input_0) + $signed(io_input_4));
  assign _zz_2 = ($signed(io_input_1) + $signed(io_input_5));
  assign _zz_3 = ($signed(io_input_0) - $signed(io_input_4));
  assign _zz_4 = ($signed(io_input_1) - $signed(io_input_5));
  assign _zz_5 = ($signed(io_input_2) + $signed(io_input_6));
  assign _zz_6 = ($signed(io_input_3) + $signed(io_input_7));
  assign _zz_7 = ($signed(io_input_2) - $signed(io_input_6));
  assign _zz_8 = ($signed(_zz_9) - $signed(_zz_10));
  assign io_output_0 = ($signed(_zz_1) + $signed(_zz_5));
  assign io_output_1 = ($signed(_zz_2) + $signed(_zz_6));
  assign io_output_2 = ($signed(_zz_3) + $signed(_zz_8));
  assign io_output_3 = ($signed(_zz_4) + $signed(_zz_7));
  assign io_output_4 = ($signed(_zz_1) - $signed(_zz_5));
  assign io_output_5 = ($signed(_zz_2) - $signed(_zz_6));
  assign io_output_6 = ($signed(_zz_3) - $signed(_zz_8));
  assign io_output_7 = ($signed(_zz_4) - $signed(_zz_7));

endmodule
