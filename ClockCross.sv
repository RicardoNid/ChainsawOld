// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : ClockCross
// Git hash  : faa40885e164272f61221e4440a1cea60f1bc866



module ClockCross (
  input               io_clkLow,
  input               io_rstLow,
  input      [7:0]    io_lowFreqInput_0,
  input      [7:0]    io_lowFreqInput_1,
  input      [7:0]    io_lowFreqInput_2,
  input      [7:0]    io_lowFreqInput_3,
  input               io_clkHigh,
  input               io_rstHigh,
  output     [7:0]    io_HighFreqOutput
);
  wire       [7:0]    _zz_1;
  reg        [7:0]    _zz_2;
  wire       [7:0]    bufferCC_1_io_dataOut;
  wire       [0:0]    _zz_3;
  wire       [1:0]    _zz_4;
  reg        [7:0]    areaLow_reg_0;
  reg        [7:0]    areaLow_reg_1;
  reg        [7:0]    areaLow_reg_2;
  reg        [7:0]    areaLow_reg_3;
  reg                 areaHigh_selCounter_willIncrement;
  wire                areaHigh_selCounter_willClear;
  reg        [1:0]    areaHigh_selCounter_valueNext;
  reg        [1:0]    areaHigh_selCounter_value;
  wire                areaHigh_selCounter_willOverflowIfInc;
  wire                areaHigh_selCounter_willOverflow;
  wire       [7:0]    areaHigh_buf;
  function  zz_areaHigh_selCounter_willIncrement(input dummy);
    begin
      zz_areaHigh_selCounter_willIncrement = 1'b0;
      if(1'b1)begin
        zz_areaHigh_selCounter_willIncrement = 1'b1;
      end
    end
  endfunction
  wire  _zz_5;

  assign _zz_3 = areaHigh_selCounter_willIncrement;
  assign _zz_4 = {1'd0, _zz_3};
  BufferCC bufferCC_1 (
    .io_dataIn     (_zz_1[7:0]                  ), //i
    .io_dataOut    (bufferCC_1_io_dataOut[7:0]  ), //o
    .io_clkHigh    (io_clkHigh                  ), //i
    .io_rstHigh    (io_rstHigh                  )  //i
  );
  always @(*) begin
    case(areaHigh_selCounter_value)
      2'b00 : begin
        _zz_2 = areaLow_reg_0;
      end
      2'b01 : begin
        _zz_2 = areaLow_reg_1;
      end
      2'b10 : begin
        _zz_2 = areaLow_reg_2;
      end
      default : begin
        _zz_2 = areaLow_reg_3;
      end
    endcase
  end

  assign _zz_5 = zz_areaHigh_selCounter_willIncrement(1'b0);
  always @ (*) areaHigh_selCounter_willIncrement = _zz_5;
  assign areaHigh_selCounter_willClear = 1'b0;
  assign areaHigh_selCounter_willOverflowIfInc = (areaHigh_selCounter_value == 2'b11);
  assign areaHigh_selCounter_willOverflow = (areaHigh_selCounter_willOverflowIfInc && areaHigh_selCounter_willIncrement);
  always @ (*) begin
    areaHigh_selCounter_valueNext = (areaHigh_selCounter_value + _zz_4);
    if(areaHigh_selCounter_willClear)begin
      areaHigh_selCounter_valueNext = 2'b00;
    end
  end

  assign _zz_1 = _zz_2;
  assign areaHigh_buf = bufferCC_1_io_dataOut;
  assign io_HighFreqOutput = areaHigh_buf;
  always @ (posedge io_clkLow) begin
    areaLow_reg_0 <= io_lowFreqInput_0;
    areaLow_reg_1 <= io_lowFreqInput_1;
    areaLow_reg_2 <= io_lowFreqInput_2;
    areaLow_reg_3 <= io_lowFreqInput_3;
  end

  always @ (posedge io_clkHigh or posedge io_rstHigh) begin
    if (io_rstHigh) begin
      areaHigh_selCounter_value <= 2'b00;
    end else begin
      areaHigh_selCounter_value <= areaHigh_selCounter_valueNext;
    end
  end


endmodule

module BufferCC (
  input      [7:0]    io_dataIn,
  output     [7:0]    io_dataOut,
  input               io_clkHigh,
  input               io_rstHigh
);
  reg        [7:0]    buffers_0;
  reg        [7:0]    buffers_1;

  assign io_dataOut = buffers_1;
  always @ (posedge io_clkHigh) begin
    buffers_0 <= io_dataIn;
    buffers_1 <= buffers_0;
  end


endmodule
