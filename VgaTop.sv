// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : VgaTop
// Git hash  : ddb8a8562f4af4bff7953d0a6d11f6affc0d2cf7



module VgaTop (
  input               io_reset,
  output              io_error,
  output              io_frameStart,
  output              io_vga_vSync,
  output              io_vga_hSync,
  output              io_vga_colorEn,
  output     [7:0]    io_vga_color_r,
  output     [7:0]    io_vga_color_g,
  output     [7:0]    io_vga_color_b,
  input               clk,
  input               reset
);
  wire       [11:0]   _zz_1;
  wire       [11:0]   _zz_2;
  wire       [11:0]   _zz_3;
  wire       [11:0]   _zz_4;
  wire       [11:0]   _zz_5;
  wire       [11:0]   _zz_6;
  wire       [11:0]   _zz_7;
  wire       [11:0]   _zz_8;
  wire                _zz_9;
  wire       [7:0]    _zz_10;
  wire       [7:0]    _zz_11;
  wire       [7:0]    _zz_12;
  reg        [31:0]   _zz_13;
  wire                ctrl_io_pixels_ready;
  wire                ctrl_io_error;
  wire                ctrl_io_frameStart;
  wire                ctrl_io_vga_vSync;
  wire                ctrl_io_vga_hSync;
  wire                ctrl_io_vga_colorEn;
  wire       [7:0]    ctrl_io_vga_color_r;
  wire       [7:0]    ctrl_io_vga_color_g;
  wire       [7:0]    ctrl_io_vga_color_b;
  wire       [0:0]    _zz_14;
  wire       [18:0]   _zz_15;
  wire                _zz_16;
  reg                 count_willIncrement;
  wire                count_willClear;
  reg        [18:0]   count_valueNext;
  reg        [18:0]   count_value;
  wire                count_willOverflowIfInc;
  wire                count_willOverflow;
  wire       [31:0]   pixel;
  reg [31:0] rom [0:307199];
  function  zz_count_willIncrement(input dummy);
    begin
      zz_count_willIncrement = 1'b0;
      if(1'b1)begin
        zz_count_willIncrement = 1'b1;
      end
    end
  endfunction
  wire  _zz_17;

  assign _zz_14 = count_willIncrement;
  assign _zz_15 = {18'd0, _zz_14};
  assign _zz_16 = 1'b1;
  initial begin
    $readmemb("VgaTop.sv_toplevel_rom.bin",rom);
  end
  always @ (posedge clk) begin
    if(_zz_16) begin
      _zz_13 <= rom[count_value];
    end
  end

  VgaCtrl ctrl (
    .io_softReset               (io_reset                  ), //i
    .io_timings_h_colorStart    (_zz_1[11:0]               ), //i
    .io_timings_h_colorEnd      (_zz_2[11:0]               ), //i
    .io_timings_h_syncStart     (_zz_3[11:0]               ), //i
    .io_timings_h_syncEnd       (_zz_4[11:0]               ), //i
    .io_timings_v_colorStart    (_zz_5[11:0]               ), //i
    .io_timings_v_colorEnd      (_zz_6[11:0]               ), //i
    .io_timings_v_syncStart     (_zz_7[11:0]               ), //i
    .io_timings_v_syncEnd       (_zz_8[11:0]               ), //i
    .io_pixels_valid            (_zz_9                     ), //i
    .io_pixels_ready            (ctrl_io_pixels_ready      ), //o
    .io_pixels_payload_r        (_zz_10[7:0]               ), //i
    .io_pixels_payload_g        (_zz_11[7:0]               ), //i
    .io_pixels_payload_b        (_zz_12[7:0]               ), //i
    .io_error                   (ctrl_io_error             ), //o
    .io_frameStart              (ctrl_io_frameStart        ), //o
    .io_vga_vSync               (ctrl_io_vga_vSync         ), //o
    .io_vga_hSync               (ctrl_io_vga_hSync         ), //o
    .io_vga_colorEn             (ctrl_io_vga_colorEn       ), //o
    .io_vga_color_r             (ctrl_io_vga_color_r[7:0]  ), //o
    .io_vga_color_g             (ctrl_io_vga_color_g[7:0]  ), //o
    .io_vga_color_b             (ctrl_io_vga_color_b[7:0]  ), //o
    .clk                        (clk                       ), //i
    .reset                      (reset                     )  //i
  );
  assign _zz_3 = 12'h05f;
  assign _zz_4 = 12'h31f;
  assign _zz_1 = 12'h06f;
  assign _zz_2 = 12'h2ef;
  assign _zz_7 = 12'h001;
  assign _zz_8 = 12'h20c;
  assign _zz_5 = 12'h00b;
  assign _zz_6 = 12'h1eb;
  assign io_error = ctrl_io_error;
  assign io_frameStart = ctrl_io_frameStart;
  assign io_vga_vSync = ctrl_io_vga_vSync;
  assign io_vga_hSync = ctrl_io_vga_hSync;
  assign io_vga_colorEn = ctrl_io_vga_colorEn;
  assign io_vga_color_r = ctrl_io_vga_color_r;
  assign io_vga_color_g = ctrl_io_vga_color_g;
  assign io_vga_color_b = ctrl_io_vga_color_b;
  assign _zz_17 = zz_count_willIncrement(1'b0);
  always @ (*) count_willIncrement = _zz_17;
  assign count_willClear = 1'b0;
  assign count_willOverflowIfInc = (count_value == 19'h4afff);
  assign count_willOverflow = (count_willOverflowIfInc && count_willIncrement);
  always @ (*) begin
    if(count_willOverflow)begin
      count_valueNext = 19'h0;
    end else begin
      count_valueNext = (count_value + _zz_15);
    end
    if(count_willClear)begin
      count_valueNext = 19'h0;
    end
  end

  assign pixel = _zz_13;
  assign _zz_10 = pixel[31 : 24];
  assign _zz_11 = pixel[23 : 16];
  assign _zz_12 = pixel[15 : 8];
  assign _zz_9 = 1'b1;
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      count_value <= 19'h0;
    end else begin
      count_value <= count_valueNext;
    end
  end


endmodule

module VgaCtrl (
  input               io_softReset,
  input      [11:0]   io_timings_h_colorStart,
  input      [11:0]   io_timings_h_colorEnd,
  input      [11:0]   io_timings_h_syncStart,
  input      [11:0]   io_timings_h_syncEnd,
  input      [11:0]   io_timings_v_colorStart,
  input      [11:0]   io_timings_v_colorEnd,
  input      [11:0]   io_timings_v_syncStart,
  input      [11:0]   io_timings_v_syncEnd,
  input               io_pixels_valid,
  output              io_pixels_ready,
  input      [7:0]    io_pixels_payload_r,
  input      [7:0]    io_pixels_payload_g,
  input      [7:0]    io_pixels_payload_b,
  output              io_error,
  output              io_frameStart,
  output              io_vga_vSync,
  output              io_vga_hSync,
  output              io_vga_colorEn,
  output     [7:0]    io_vga_color_r,
  output     [7:0]    io_vga_color_g,
  output     [7:0]    io_vga_color_b,
  input               clk,
  input               reset
);
  reg        [11:0]   h_counter;
  wire                h_syncStart;
  wire                h_syncEnd;
  wire                h_colorStart;
  wire                h_colorEnd;
  reg                 h_sync;
  reg                 h_colorEn;
  reg        [11:0]   v_counter;
  wire                v_syncStart;
  wire                v_syncEnd;
  wire                v_colorStart;
  wire                v_colorEnd;
  reg                 v_sync;
  reg                 v_colorEn;
  wire                colorEn;

  assign h_syncStart = (h_counter == io_timings_h_syncStart);
  assign h_syncEnd = (h_counter == io_timings_h_syncEnd);
  assign h_colorStart = (h_counter == io_timings_h_colorStart);
  assign h_colorEnd = (h_counter == io_timings_h_colorEnd);
  assign v_syncStart = (v_counter == io_timings_v_syncStart);
  assign v_syncEnd = (v_counter == io_timings_v_syncEnd);
  assign v_colorStart = (v_counter == io_timings_v_colorStart);
  assign v_colorEnd = (v_counter == io_timings_v_colorEnd);
  assign colorEn = (h_colorEn && v_colorEn);
  assign io_pixels_ready = colorEn;
  assign io_error = (colorEn && io_pixels_valid);
  assign io_frameStart = v_syncEnd;
  assign io_vga_hSync = h_sync;
  assign io_vga_vSync = v_sync;
  assign io_vga_colorEn = colorEn;
  assign io_vga_color_r = io_pixels_payload_r;
  assign io_vga_color_g = io_pixels_payload_g;
  assign io_vga_color_b = io_pixels_payload_b;
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      h_counter <= 12'h0;
      h_sync <= 1'b0;
      h_colorEn <= 1'b0;
      v_counter <= 12'h0;
      v_sync <= 1'b0;
      v_colorEn <= 1'b0;
    end else begin
      if(1'b1)begin
        h_counter <= (h_counter + 12'h001);
        if(h_syncEnd)begin
          h_counter <= 12'h0;
        end
      end
      if(h_syncStart)begin
        h_sync <= 1'b1;
      end
      if(h_syncEnd)begin
        h_sync <= 1'b0;
      end
      if(h_colorStart)begin
        h_colorEn <= 1'b1;
      end
      if(h_colorEnd)begin
        h_colorEn <= 1'b0;
      end
      if(io_softReset)begin
        h_counter <= 12'h0;
        h_sync <= 1'b0;
        h_colorEn <= 1'b0;
      end
      if(h_syncEnd)begin
        v_counter <= (v_counter + 12'h001);
        if(v_syncEnd)begin
          v_counter <= 12'h0;
        end
      end
      if(v_syncStart)begin
        v_sync <= 1'b1;
      end
      if(v_syncEnd)begin
        v_sync <= 1'b0;
      end
      if(v_colorStart)begin
        v_colorEn <= 1'b1;
      end
      if(v_colorEnd)begin
        v_colorEn <= 1'b0;
      end
      if(io_softReset)begin
        v_counter <= 12'h0;
        v_sync <= 1'b0;
        v_colorEn <= 1'b0;
      end
    end
  end


endmodule
