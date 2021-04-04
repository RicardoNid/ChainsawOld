// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : SlowAreaExample
// Git hash  : c48da7eaa63608d19b2793f5e3644f678b727d2c



module SlowAreaExample (
  output     [3:0]    io_o1,
  output     [3:0]    io_o2,
  input               clk,
  input               reset
);
  wire       [0:0]    _zz_8;
  wire       [3:0]    _zz_9;
  wire       [0:0]    _zz_10;
  wire       [3:0]    _zz_11;
  reg                 counter1_willIncrement;
  wire                counter1_willClear;
  reg        [3:0]    counter1_valueNext;
  reg        [3:0]    counter1_value;
  wire                counter1_willOverflowIfInc;
  wire                counter1_willOverflow;
  reg        [1:0]    _zz_1;
  wire                _zz_2;
  reg                 _zz_2_regNext;
  reg                 _zz_3;
  reg        [3:0]    _zz_4;
  reg        [3:0]    _zz_5;
  wire                _zz_6;
  wire                _zz_7;
  function  zz_counter1_willIncrement(input dummy);
    begin
      zz_counter1_willIncrement = 1'b0;
      zz_counter1_willIncrement = 1'b1;
    end
  endfunction
  wire  _zz_12;
  function  zz__zz_3(input dummy);
    begin
      zz__zz_3 = 1'b0;
      zz__zz_3 = 1'b1;
    end
  endfunction
  wire  _zz_13;

  assign _zz_8 = counter1_willIncrement;
  assign _zz_9 = {3'd0, _zz_8};
  assign _zz_10 = _zz_3;
  assign _zz_11 = {3'd0, _zz_10};
  assign _zz_12 = zz_counter1_willIncrement(1'b0);
  always @ (*) counter1_willIncrement = _zz_12;
  assign counter1_willClear = 1'b0;
  assign counter1_willOverflowIfInc = (counter1_value == 4'b1001);
  assign counter1_willOverflow = (counter1_willOverflowIfInc && counter1_willIncrement);
  always @ (*) begin
    if(counter1_willOverflow)begin
      counter1_valueNext = 4'b0000;
    end else begin
      counter1_valueNext = (counter1_value + _zz_9);
    end
    if(counter1_willClear)begin
      counter1_valueNext = 4'b0000;
    end
  end

  assign io_o1 = counter1_value;
  assign _zz_2 = (_zz_1 == 2'b11);
  assign _zz_13 = zz__zz_3(1'b0);
  always @ (*) _zz_3 = _zz_13;
  assign _zz_6 = (_zz_5 == 4'b1001);
  assign _zz_7 = (_zz_6 && _zz_3);
  always @ (*) begin
    if(_zz_7)begin
      _zz_4 = 4'b0000;
    end else begin
      _zz_4 = (_zz_5 + _zz_11);
    end
    if(1'b0)begin
      _zz_4 = 4'b0000;
    end
  end

  assign io_o2 = _zz_5;
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      counter1_value <= 4'b0000;
      _zz_1 <= 2'b00;
      _zz_2_regNext <= 1'b0;
    end else begin
      counter1_value <= counter1_valueNext;
      _zz_1 <= (_zz_1 + 2'b01);
      if(_zz_2)begin
        _zz_1 <= 2'b00;
      end
      _zz_2_regNext <= _zz_2;
    end
  end

  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      _zz_5 <= 4'b0000;
    end else begin
      if(_zz_2_regNext) begin
        _zz_5 <= _zz_4;
      end
    end
  end


endmodule
