// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : StaticComplexMultiplier
// Git hash  : d7943635f0317afaf10af76ed9307a372bbef1fa



module StaticComplexMultiplier (
  input      [16:0]   io_X,
  input      [16:0]   io_Y,
  output     [16:0]   io_R,
  output     [16:0]   io_I
);
  wire       [25:0]   _zz_1;
  wire       [33:0]   _zz_2;
  wire       [33:0]   _zz_3;
  wire       [16:0]   _zz_4;
  wire       [25:0]   _zz_5;
  wire       [33:0]   _zz_6;
  wire       [33:0]   _zz_7;
  wire       [16:0]   _zz_8;
  wire       [16:0]   Cfix;
  wire       [16:0]   Sfix;
  wire       [16:0]   E;
  wire       [33:0]   Z;

  assign _zz_1 = (_zz_2 >>> 8);
  assign _zz_2 = ($signed(_zz_3) + $signed(Z));
  assign _zz_3 = ($signed(_zz_4) * $signed(io_Y));
  assign _zz_4 = ($signed(Cfix) - $signed(Sfix));
  assign _zz_5 = (_zz_6 >>> 8);
  assign _zz_6 = ($signed(_zz_7) + $signed(Z));
  assign _zz_7 = ($signed(_zz_8) * $signed(io_X));
  assign _zz_8 = ($signed(Cfix) + $signed(Sfix));
  assign Cfix = 17'h00140;
  assign Sfix = 17'h00240;
  assign E = ($signed(io_X) - $signed(io_Y));
  assign Z = ($signed(Cfix) * $signed(E));
  assign io_R = _zz_1[16:0];
  assign io_I = _zz_5[16:0];

endmodule
