// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : ColorSumming
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815



module ColorSumming (
  input      [3:0]    io_sources_0_r,
  input      [3:0]    io_sources_0_g,
  input      [3:0]    io_sources_0_b,
  input      [3:0]    io_sources_1_r,
  input      [3:0]    io_sources_1_g,
  input      [3:0]    io_sources_1_b,
  input      [3:0]    io_sources_2_r,
  input      [3:0]    io_sources_2_g,
  input      [3:0]    io_sources_2_b,
  input      [3:0]    io_sources_3_r,
  input      [3:0]    io_sources_3_g,
  input      [3:0]    io_sources_3_b,
  input      [3:0]    io_sources_4_r,
  input      [3:0]    io_sources_4_g,
  input      [3:0]    io_sources_4_b,
  input      [3:0]    io_sources_5_r,
  input      [3:0]    io_sources_5_g,
  input      [3:0]    io_sources_5_b,
  input      [3:0]    io_sources_6_r,
  input      [3:0]    io_sources_6_g,
  input      [3:0]    io_sources_6_b,
  input      [3:0]    io_sources_7_r,
  input      [3:0]    io_sources_7_g,
  input      [3:0]    io_sources_7_b,
  output     [3:0]    io_result_r,
  output     [3:0]    io_result_g,
  output     [3:0]    io_result_b
);
  reg        [3:0]    _zz_1;
  reg        [3:0]    _zz_2;
  reg        [3:0]    _zz_3;
  reg        [3:0]    _zz_4;
  reg        [3:0]    _zz_5;
  reg        [3:0]    _zz_6;
  reg        [3:0]    _zz_7;
  reg        [3:0]    _zz_8;
  reg        [3:0]    _zz_9;
  reg        [3:0]    _zz_10;
  reg        [3:0]    _zz_11;
  reg        [3:0]    _zz_12;
  reg        [3:0]    _zz_13;
  reg        [3:0]    _zz_14;
  reg        [3:0]    _zz_15;
  reg        [3:0]    _zz_16;
  reg        [3:0]    _zz_17;
  reg        [3:0]    _zz_18;
  reg        [3:0]    _zz_19;
  reg        [3:0]    _zz_20;
  reg        [3:0]    _zz_21;
  reg        [3:0]    _zz_22;
  reg        [3:0]    _zz_23;
  reg        [3:0]    _zz_24;
  wire       [3:0]    sum_r;
  wire       [3:0]    sum_g;
  wire       [3:0]    sum_b;

  always @ (*) begin
    _zz_1 = _zz_4;
    _zz_1 = (_zz_4 + io_sources_7_r);
  end

  always @ (*) begin
    _zz_2 = _zz_5;
    _zz_2 = (_zz_5 + io_sources_7_g);
  end

  always @ (*) begin
    _zz_3 = _zz_6;
    _zz_3 = (_zz_6 + io_sources_7_b);
  end

  always @ (*) begin
    _zz_4 = _zz_7;
    _zz_4 = (_zz_7 + io_sources_6_r);
  end

  always @ (*) begin
    _zz_5 = _zz_8;
    _zz_5 = (_zz_8 + io_sources_6_g);
  end

  always @ (*) begin
    _zz_6 = _zz_9;
    _zz_6 = (_zz_9 + io_sources_6_b);
  end

  always @ (*) begin
    _zz_7 = _zz_10;
    _zz_7 = (_zz_10 + io_sources_5_r);
  end

  always @ (*) begin
    _zz_8 = _zz_11;
    _zz_8 = (_zz_11 + io_sources_5_g);
  end

  always @ (*) begin
    _zz_9 = _zz_12;
    _zz_9 = (_zz_12 + io_sources_5_b);
  end

  always @ (*) begin
    _zz_10 = _zz_13;
    _zz_10 = (_zz_13 + io_sources_4_r);
  end

  always @ (*) begin
    _zz_11 = _zz_14;
    _zz_11 = (_zz_14 + io_sources_4_g);
  end

  always @ (*) begin
    _zz_12 = _zz_15;
    _zz_12 = (_zz_15 + io_sources_4_b);
  end

  always @ (*) begin
    _zz_13 = _zz_16;
    _zz_13 = (_zz_16 + io_sources_3_r);
  end

  always @ (*) begin
    _zz_14 = _zz_17;
    _zz_14 = (_zz_17 + io_sources_3_g);
  end

  always @ (*) begin
    _zz_15 = _zz_18;
    _zz_15 = (_zz_18 + io_sources_3_b);
  end

  always @ (*) begin
    _zz_16 = _zz_19;
    _zz_16 = (_zz_19 + io_sources_2_r);
  end

  always @ (*) begin
    _zz_17 = _zz_20;
    _zz_17 = (_zz_20 + io_sources_2_g);
  end

  always @ (*) begin
    _zz_18 = _zz_21;
    _zz_18 = (_zz_21 + io_sources_2_b);
  end

  always @ (*) begin
    _zz_19 = _zz_22;
    _zz_19 = (_zz_22 + io_sources_1_r);
  end

  always @ (*) begin
    _zz_20 = _zz_23;
    _zz_20 = (_zz_23 + io_sources_1_g);
  end

  always @ (*) begin
    _zz_21 = _zz_24;
    _zz_21 = (_zz_24 + io_sources_1_b);
  end

  always @ (*) begin
    _zz_22 = sum_r;
    _zz_22 = (sum_r + io_sources_0_r);
  end

  always @ (*) begin
    _zz_23 = sum_g;
    _zz_23 = (sum_g + io_sources_0_g);
  end

  always @ (*) begin
    _zz_24 = sum_b;
    _zz_24 = (sum_b + io_sources_0_b);
  end

  assign sum_r = 4'b0000;
  assign sum_g = 4'b0000;
  assign sum_b = 4'b0000;
  assign io_result_r = _zz_1;
  assign io_result_g = _zz_2;
  assign io_result_b = _zz_3;

endmodule
