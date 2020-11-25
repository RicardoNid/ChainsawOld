// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : Timer
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815



module Timer (
  output              io_tick,
  input               clk,
  input               reset
);
  wire       [0:0]    _zz_1;
  wire       [3:0]    _zz_2;
  reg                 timer_state;
  reg                 timer_stateRise;
  wire                timer_counter_willIncrement;
  reg                 timer_counter_willClear;
  reg        [3:0]    timer_counter_valueNext;
  reg        [3:0]    timer_counter_value;
  wire                timer_counter_willOverflowIfInc;
  wire                timer_counter_willOverflow;

  assign _zz_1 = timer_counter_willIncrement;
  assign _zz_2 = {3'd0, _zz_1};
  always @ (*) begin
    timer_stateRise = 1'b0;
    if(timer_counter_willOverflow)begin
      timer_stateRise = (! timer_state);
    end
    if(io_tick)begin
      timer_stateRise = 1'b0;
    end
  end

  always @ (*) begin
    timer_counter_willClear = 1'b0;
    if(io_tick)begin
      timer_counter_willClear = 1'b1;
    end
  end

  assign timer_counter_willOverflowIfInc = (timer_counter_value == 4'b1001);
  assign timer_counter_willOverflow = (timer_counter_willOverflowIfInc && timer_counter_willIncrement);
  always @ (*) begin
    if(timer_counter_willOverflow)begin
      timer_counter_valueNext = 4'b0000;
    end else begin
      timer_counter_valueNext = (timer_counter_value + _zz_2);
    end
    if(timer_counter_willClear)begin
      timer_counter_valueNext = 4'b0000;
    end
  end

  assign timer_counter_willIncrement = 1'b1;
  assign io_tick = timer_state;
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      timer_state <= 1'b0;
      timer_counter_value <= 4'b0000;
    end else begin
      timer_counter_value <= timer_counter_valueNext;
      if(timer_counter_willOverflow)begin
        timer_state <= 1'b1;
      end
      if(io_tick)begin
        timer_state <= 1'b0;
      end
    end
  end


endmodule
