// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : CooleyTukeyFFT_2
// Git hash  : 0ddbc7657b05706f03c0a8a363d7dc0d9d001858



module CooleyTukeyFFT_2 (
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
  wire       [16:0]   _zz_13;
  wire       [16:0]   _zz_14;
  wire       [16:0]   _zz_15;
  wire       [16:0]   _zz_16;
  wire       [16:0]   _zz_17;
  wire       [16:0]   _zz_18;
  wire       [16:0]   _zz_19;
  wire       [16:0]   _zz_20;
  wire       [16:0]   winogradDFT_4_io_output_0;
  wire       [16:0]   winogradDFT_4_io_output_1;
  wire       [16:0]   winogradDFT_4_io_output_2;
  wire       [16:0]   winogradDFT_4_io_output_3;
  wire       [16:0]   winogradDFT_5_io_output_0;
  wire       [16:0]   winogradDFT_5_io_output_1;
  wire       [16:0]   winogradDFT_5_io_output_2;
  wire       [16:0]   winogradDFT_5_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_3_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_3_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_3_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_3_io_output_3;
  wire       [16:0]   cooleyTukeyFFT_4_io_output_0;
  wire       [16:0]   cooleyTukeyFFT_4_io_output_1;
  wire       [16:0]   cooleyTukeyFFT_4_io_output_2;
  wire       [16:0]   cooleyTukeyFFT_4_io_output_3;
  wire       [16:0]   _zz_21;
  wire       [25:0]   _zz_22;
  wire       [33:0]   _zz_23;
  wire       [33:0]   _zz_24;
  wire       [16:0]   _zz_25;
  wire       [25:0]   _zz_26;
  wire       [33:0]   _zz_27;
  wire       [33:0]   _zz_28;
  wire       [16:0]   _zz_29;
  wire       [16:0]   _zz_30;
  wire       [25:0]   _zz_31;
  wire       [33:0]   _zz_32;
  wire       [33:0]   _zz_33;
  wire       [16:0]   _zz_34;
  wire       [25:0]   _zz_35;
  wire       [33:0]   _zz_36;
  wire       [33:0]   _zz_37;
  wire       [16:0]   _zz_38;
  wire       [16:0]   _zz_39;
  wire       [25:0]   _zz_40;
  wire       [33:0]   _zz_41;
  wire       [33:0]   _zz_42;
  wire       [16:0]   _zz_43;
  wire       [25:0]   _zz_44;
  wire       [33:0]   _zz_45;
  wire       [33:0]   _zz_46;
  wire       [16:0]   _zz_47;
  wire       [16:0]   _zz_48;
  wire       [25:0]   _zz_49;
  wire       [33:0]   _zz_50;
  wire       [33:0]   _zz_51;
  wire       [16:0]   _zz_52;
  wire       [25:0]   _zz_53;
  wire       [33:0]   _zz_54;
  wire       [33:0]   _zz_55;
  wire       [16:0]   _zz_56;
  wire       [16:0]   _zz_1;
  wire       [16:0]   _zz_2;
  wire       [33:0]   _zz_3;
  wire       [16:0]   _zz_4;
  wire       [16:0]   _zz_5;
  wire       [33:0]   _zz_6;
  wire       [16:0]   _zz_7;
  wire       [16:0]   _zz_8;
  wire       [33:0]   _zz_9;
  wire       [16:0]   _zz_10;
  wire       [16:0]   _zz_11;
  wire       [33:0]   _zz_12;

  assign _zz_21 = ($signed(cooleyTukeyFFT_3_io_output_0) - $signed(cooleyTukeyFFT_3_io_output_1));
  assign _zz_22 = (_zz_23 >>> 8);
  assign _zz_23 = ($signed(_zz_24) + $signed(_zz_3));
  assign _zz_24 = ($signed(_zz_25) * $signed(cooleyTukeyFFT_3_io_output_1));
  assign _zz_25 = ($signed(_zz_1) - $signed(_zz_2));
  assign _zz_26 = (_zz_27 >>> 8);
  assign _zz_27 = ($signed(_zz_28) - $signed(_zz_3));
  assign _zz_28 = ($signed(_zz_29) * $signed(cooleyTukeyFFT_3_io_output_0));
  assign _zz_29 = ($signed(_zz_1) + $signed(_zz_2));
  assign _zz_30 = ($signed(cooleyTukeyFFT_4_io_output_0) - $signed(cooleyTukeyFFT_4_io_output_1));
  assign _zz_31 = (_zz_32 >>> 8);
  assign _zz_32 = ($signed(_zz_33) + $signed(_zz_6));
  assign _zz_33 = ($signed(_zz_34) * $signed(cooleyTukeyFFT_4_io_output_1));
  assign _zz_34 = ($signed(_zz_4) - $signed(_zz_5));
  assign _zz_35 = (_zz_36 >>> 8);
  assign _zz_36 = ($signed(_zz_37) - $signed(_zz_6));
  assign _zz_37 = ($signed(_zz_38) * $signed(cooleyTukeyFFT_4_io_output_0));
  assign _zz_38 = ($signed(_zz_4) + $signed(_zz_5));
  assign _zz_39 = ($signed(cooleyTukeyFFT_3_io_output_2) - $signed(cooleyTukeyFFT_3_io_output_3));
  assign _zz_40 = (_zz_41 >>> 8);
  assign _zz_41 = ($signed(_zz_42) + $signed(_zz_9));
  assign _zz_42 = ($signed(_zz_43) * $signed(cooleyTukeyFFT_3_io_output_3));
  assign _zz_43 = ($signed(_zz_7) - $signed(_zz_8));
  assign _zz_44 = (_zz_45 >>> 8);
  assign _zz_45 = ($signed(_zz_46) - $signed(_zz_9));
  assign _zz_46 = ($signed(_zz_47) * $signed(cooleyTukeyFFT_3_io_output_2));
  assign _zz_47 = ($signed(_zz_7) + $signed(_zz_8));
  assign _zz_48 = ($signed(cooleyTukeyFFT_4_io_output_2) - $signed(cooleyTukeyFFT_4_io_output_3));
  assign _zz_49 = (_zz_50 >>> 8);
  assign _zz_50 = ($signed(_zz_51) + $signed(_zz_12));
  assign _zz_51 = ($signed(_zz_52) * $signed(cooleyTukeyFFT_4_io_output_3));
  assign _zz_52 = ($signed(_zz_10) - $signed(_zz_11));
  assign _zz_53 = (_zz_54 >>> 8);
  assign _zz_54 = ($signed(_zz_55) - $signed(_zz_12));
  assign _zz_55 = ($signed(_zz_56) * $signed(cooleyTukeyFFT_4_io_output_2));
  assign _zz_56 = ($signed(_zz_10) + $signed(_zz_11));
  WinogradDFT winogradDFT_4 (
    .io_input_0     (_zz_13[16:0]                     ), //i
    .io_input_1     (_zz_14[16:0]                     ), //i
    .io_input_2     (_zz_15[16:0]                     ), //i
    .io_input_3     (_zz_16[16:0]                     ), //i
    .io_output_0    (winogradDFT_4_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_4_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_4_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_4_io_output_3[16:0]  )  //o
  );
  WinogradDFT winogradDFT_5 (
    .io_input_0     (_zz_17[16:0]                     ), //i
    .io_input_1     (_zz_18[16:0]                     ), //i
    .io_input_2     (_zz_19[16:0]                     ), //i
    .io_input_3     (_zz_20[16:0]                     ), //i
    .io_output_0    (winogradDFT_5_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_5_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_5_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_5_io_output_3[16:0]  )  //o
  );
  CooleyTukeyFFT cooleyTukeyFFT_3 (
    .io_input_0     (io_input_0[16:0]                    ), //i
    .io_input_1     (io_input_1[16:0]                    ), //i
    .io_input_2     (io_input_2[16:0]                    ), //i
    .io_input_3     (io_input_3[16:0]                    ), //i
    .io_output_0    (cooleyTukeyFFT_3_io_output_0[16:0]  ), //o
    .io_output_1    (cooleyTukeyFFT_3_io_output_1[16:0]  ), //o
    .io_output_2    (cooleyTukeyFFT_3_io_output_2[16:0]  ), //o
    .io_output_3    (cooleyTukeyFFT_3_io_output_3[16:0]  )  //o
  );
  CooleyTukeyFFT cooleyTukeyFFT_4 (
    .io_input_0     (io_input_4[16:0]                    ), //i
    .io_input_1     (io_input_5[16:0]                    ), //i
    .io_input_2     (io_input_6[16:0]                    ), //i
    .io_input_3     (io_input_7[16:0]                    ), //i
    .io_output_0    (cooleyTukeyFFT_4_io_output_0[16:0]  ), //o
    .io_output_1    (cooleyTukeyFFT_4_io_output_1[16:0]  ), //o
    .io_output_2    (cooleyTukeyFFT_4_io_output_2[16:0]  ), //o
    .io_output_3    (cooleyTukeyFFT_4_io_output_3[16:0]  )  //o
  );
  assign _zz_1 = 17'h00100;
  assign _zz_2 = 17'h0;
  assign _zz_3 = ($signed(_zz_1) * $signed(_zz_21));
  assign _zz_13 = _zz_22[16:0];
  assign _zz_14 = _zz_26[16:0];
  assign _zz_4 = 17'h00100;
  assign _zz_5 = 17'h0;
  assign _zz_6 = ($signed(_zz_4) * $signed(_zz_30));
  assign _zz_15 = _zz_31[16:0];
  assign _zz_16 = _zz_35[16:0];
  assign _zz_7 = 17'h00100;
  assign _zz_8 = 17'h0;
  assign _zz_9 = ($signed(_zz_7) * $signed(_zz_39));
  assign _zz_17 = _zz_40[16:0];
  assign _zz_18 = _zz_44[16:0];
  assign _zz_10 = 17'h0;
  assign _zz_11 = 17'h1ff00;
  assign _zz_12 = ($signed(_zz_10) * $signed(_zz_48));
  assign _zz_19 = _zz_49[16:0];
  assign _zz_20 = _zz_53[16:0];
  assign io_output_0 = winogradDFT_4_io_output_0;
  assign io_output_1 = winogradDFT_4_io_output_1;
  assign io_output_2 = winogradDFT_4_io_output_2;
  assign io_output_3 = winogradDFT_4_io_output_3;
  assign io_output_4 = winogradDFT_5_io_output_0;
  assign io_output_5 = winogradDFT_5_io_output_1;
  assign io_output_6 = winogradDFT_5_io_output_2;
  assign io_output_7 = winogradDFT_5_io_output_3;

endmodule

//CooleyTukeyFFT replaced by CooleyTukeyFFT

module CooleyTukeyFFT (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3
);
  wire       [16:0]   winogradDFT_4_io_output_0;
  wire       [16:0]   winogradDFT_4_io_output_1;
  wire       [16:0]   winogradDFT_4_io_output_2;
  wire       [16:0]   winogradDFT_4_io_output_3;

  WinogradDFT winogradDFT_4 (
    .io_input_0     (io_input_0[16:0]                 ), //i
    .io_input_1     (io_input_1[16:0]                 ), //i
    .io_input_2     (io_input_2[16:0]                 ), //i
    .io_input_3     (io_input_3[16:0]                 ), //i
    .io_output_0    (winogradDFT_4_io_output_0[16:0]  ), //o
    .io_output_1    (winogradDFT_4_io_output_1[16:0]  ), //o
    .io_output_2    (winogradDFT_4_io_output_2[16:0]  ), //o
    .io_output_3    (winogradDFT_4_io_output_3[16:0]  )  //o
  );
  assign io_output_0 = winogradDFT_4_io_output_0;
  assign io_output_1 = winogradDFT_4_io_output_1;
  assign io_output_2 = winogradDFT_4_io_output_2;
  assign io_output_3 = winogradDFT_4_io_output_3;

endmodule

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

//WinogradDFT replaced by WinogradDFT

module WinogradDFT (
  input      [16:0]   io_input_0,
  input      [16:0]   io_input_1,
  input      [16:0]   io_input_2,
  input      [16:0]   io_input_3,
  output     [16:0]   io_output_0,
  output     [16:0]   io_output_1,
  output     [16:0]   io_output_2,
  output     [16:0]   io_output_3
);

  assign io_output_0 = ($signed(io_input_0) + $signed(io_input_2));
  assign io_output_1 = ($signed(io_input_1) + $signed(io_input_3));
  assign io_output_2 = ($signed(io_input_0) - $signed(io_input_2));
  assign io_output_3 = ($signed(io_input_1) - $signed(io_input_3));

endmodule
