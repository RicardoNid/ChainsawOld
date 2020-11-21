// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : Interleave



module Interleave (
  input               clockInput,
  input               clockOutput,
  input               rstInput,
  input               rstOutput,
  input      [7:0]    input_1,
  output     [7:0]    output_1
);
  reg        [7:0]    _zz_1;
  wire       [0:0]    _zz_2;
  wire       [11:0]   _zz_3;
  wire       [17:0]   _zz_4;
  wire       [17:0]   _zz_5;
  wire       [11:0]   _zz_6;
  wire       [11:0]   _zz_7;
  wire       [17:0]   _zz_8;
  wire       [0:0]    _zz_9;
  wire       [11:0]   _zz_10;
  wire       [18:0]   _zz_11;
  wire       [18:0]   _zz_12;
  wire       [11:0]   _zz_13;
  wire       [11:0]   _zz_14;
  wire       [18:0]   _zz_15;
  wire                _zz_16;
  wire       [7:0]    _zz_17;
  wire                _zz_18;
  reg                 areaRd_counterRd_willIncrement;
  wire                areaRd_counterRd_willClear;
  reg        [11:0]   areaRd_counterRd_valueNext;
  reg        [11:0]   areaRd_counterRd_value;
  wire                areaRd_counterRd_willOverflowIfInc;
  wire                areaRd_counterRd_willOverflow;
  wire       [10:0]   areaRd_addressRd;
  reg                 areaWr_counterWr_willIncrement;
  wire                areaWr_counterWr_willClear;
  reg        [11:0]   areaWr_counterWr_valueNext;
  reg        [11:0]   areaWr_counterWr_value;
  wire                areaWr_counterWr_willOverflowIfInc;
  wire                areaWr_counterWr_willOverflow;
  wire       [10:0]   areaWr_addressWr;
  reg [7:0] ping [0:2047];
  reg [7:0] pong [0:2047];
  function  zz_areaRd_counterRd_willIncrement(input dummy);
    begin
      zz_areaRd_counterRd_willIncrement = 1'b0;
      if(1'b1)begin
        zz_areaRd_counterRd_willIncrement = 1'b1;
      end
    end
  endfunction
  wire  _zz_19;
  function  zz_areaWr_counterWr_willIncrement(input dummy);
    begin
      zz_areaWr_counterWr_willIncrement = 1'b0;
      if(1'b1)begin
        zz_areaWr_counterWr_willIncrement = 1'b1;
      end
    end
  endfunction
  wire  _zz_20;

  assign _zz_2 = areaRd_counterRd_willIncrement;
  assign _zz_3 = {11'd0, _zz_2};
  assign _zz_4 = (_zz_5 + _zz_8);
  assign _zz_5 = (_zz_6 * 6'h20);
  assign _zz_6 = (areaRd_counterRd_value % 7'h40);
  assign _zz_7 = (areaRd_counterRd_value / 7'h40);
  assign _zz_8 = {6'd0, _zz_7};
  assign _zz_9 = areaWr_counterWr_willIncrement;
  assign _zz_10 = {11'd0, _zz_9};
  assign _zz_11 = (_zz_12 + _zz_15);
  assign _zz_12 = (_zz_13 * 7'h40);
  assign _zz_13 = (areaWr_counterWr_value % 6'h20);
  assign _zz_14 = (areaWr_counterWr_value / 6'h20);
  assign _zz_15 = {7'd0, _zz_14};
  assign _zz_16 = 1'b1;
  assign _zz_17 = input_1;
  assign _zz_18 = 1'b1;
  always @ (posedge clockOutput) begin
    if(_zz_16) begin
      _zz_1 <= ping[areaRd_addressRd];
    end
  end

  always @ (posedge clockInput) begin
    if(_zz_18) begin
      ping[areaWr_addressWr] <= _zz_17;
    end
  end

  assign _zz_19 = zz_areaRd_counterRd_willIncrement(1'b0);
  always @ (*) areaRd_counterRd_willIncrement = _zz_19;
  assign areaRd_counterRd_willClear = 1'b0;
  assign areaRd_counterRd_willOverflowIfInc = (areaRd_counterRd_value == 12'h800);
  assign areaRd_counterRd_willOverflow = (areaRd_counterRd_willOverflowIfInc && areaRd_counterRd_willIncrement);
  always @ (*) begin
    if(areaRd_counterRd_willOverflow)begin
      areaRd_counterRd_valueNext = 12'h0;
    end else begin
      areaRd_counterRd_valueNext = (areaRd_counterRd_value + _zz_3);
    end
    if(areaRd_counterRd_willClear)begin
      areaRd_counterRd_valueNext = 12'h0;
    end
  end

  assign areaRd_addressRd = _zz_4[10:0];
  assign output_1 = _zz_1;
  assign _zz_20 = zz_areaWr_counterWr_willIncrement(1'b0);
  always @ (*) areaWr_counterWr_willIncrement = _zz_20;
  assign areaWr_counterWr_willClear = 1'b0;
  assign areaWr_counterWr_willOverflowIfInc = (areaWr_counterWr_value == 12'h800);
  assign areaWr_counterWr_willOverflow = (areaWr_counterWr_willOverflowIfInc && areaWr_counterWr_willIncrement);
  always @ (*) begin
    if(areaWr_counterWr_willOverflow)begin
      areaWr_counterWr_valueNext = 12'h0;
    end else begin
      areaWr_counterWr_valueNext = (areaWr_counterWr_value + _zz_10);
    end
    if(areaWr_counterWr_willClear)begin
      areaWr_counterWr_valueNext = 12'h0;
    end
  end

  assign areaWr_addressWr = _zz_11[10:0];
  always @ (posedge clockOutput or posedge rstOutput) begin
    if (rstOutput) begin
      areaRd_counterRd_value <= 12'h0;
    end else begin
      areaRd_counterRd_value <= areaRd_counterRd_valueNext;
    end
  end

  always @ (posedge clockInput or posedge rstInput) begin
    if (rstInput) begin
      areaWr_counterWr_value <= 12'h0;
    end else begin
      areaWr_counterWr_value <= areaWr_counterWr_valueNext;
    end
  end


endmodule
