// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : VgaCtrl
// Git hash  : ddb8a8562f4af4bff7953d0a6d11f6affc0d2cf7



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
