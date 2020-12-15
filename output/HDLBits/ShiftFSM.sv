// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module top_module (
  input               clk,
  input               reset,
  output              shift_ena
);
  wire       [0:0]    _zz_1;
  wire       [1:0]    _zz_2;
  reg                 mainCD_timeout_state;
  reg                 mainCD_timeout_stateRise;
  wire                mainCD_timeout_counter_willIncrement;
  wire                mainCD_timeout_counter_willClear;
  reg        [1:0]    mainCD_timeout_counter_valueNext;
  reg        [1:0]    mainCD_timeout_counter_value;
  wire                mainCD_timeout_counter_willOverflowIfInc;
  wire                mainCD_timeout_counter_willOverflow;

  assign _zz_1 = mainCD_timeout_counter_willIncrement;
  assign _zz_2 = {1'd0, _zz_1};
  always @ (*) begin
    mainCD_timeout_stateRise = 1'b0;
    if(mainCD_timeout_counter_willOverflow)begin
      mainCD_timeout_stateRise = (! mainCD_timeout_state);
    end
  end

  assign mainCD_timeout_counter_willClear = 1'b0;
  assign mainCD_timeout_counter_willOverflowIfInc = (mainCD_timeout_counter_value == 2'b11);
  assign mainCD_timeout_counter_willOverflow = (mainCD_timeout_counter_willOverflowIfInc && mainCD_timeout_counter_willIncrement);
  always @ (*) begin
    mainCD_timeout_counter_valueNext = (mainCD_timeout_counter_value + _zz_2);
    if(mainCD_timeout_counter_willClear)begin
      mainCD_timeout_counter_valueNext = 2'b00;
    end
  end

  assign mainCD_timeout_counter_willIncrement = 1'b1;
  assign shift_ena = (mainCD_timeout_state ? 1'b0 : 1'b1);
  always @ (posedge clk) begin
    if(reset) begin
      mainCD_timeout_state <= 1'b0;
      mainCD_timeout_counter_value <= 2'b00;
    end else begin
      mainCD_timeout_counter_value <= mainCD_timeout_counter_valueNext;
      if(mainCD_timeout_counter_willOverflow)begin
        mainCD_timeout_state <= 1'b1;
      end
    end
  end


endmodule
