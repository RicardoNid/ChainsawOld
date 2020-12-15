// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module top_module (
  input               clk,
  input               reset,
  input               ena,
  output     [0:0]    pm,
  output reg [7:0]    hh,
  output     [7:0]    mm,
  output     [7:0]    ss
);
  wire       [0:0]    _zz_29;
  wire       [3:0]    _zz_30;
  wire       [0:0]    _zz_31;
  wire       [2:0]    _zz_32;
  wire       [0:0]    _zz_33;
  wire       [3:0]    _zz_34;
  wire       [0:0]    _zz_35;
  wire       [2:0]    _zz_36;
  wire       [0:0]    _zz_37;
  wire       [3:0]    _zz_38;
  wire       [3:0]    _zz_39;
  wire       [3:0]    _zz_40;
  wire       [3:0]    _zz_41;
  wire       [3:0]    _zz_42;
  reg                 _zz_1;
  reg        [3:0]    _zz_2;
  reg        [3:0]    _zz_3;
  wire                _zz_4;
  wire                _zz_5;
  reg                 _zz_6;
  reg        [2:0]    _zz_7;
  reg        [2:0]    _zz_8;
  wire                _zz_9;
  wire                _zz_10;
  reg                 _zz_11;
  reg        [3:0]    _zz_12;
  reg        [3:0]    _zz_13;
  wire                _zz_14;
  wire                _zz_15;
  reg                 _zz_16;
  reg        [2:0]    _zz_17;
  reg        [2:0]    _zz_18;
  wire                _zz_19;
  wire                _zz_20;
  reg                 _zz_21;
  reg        [3:0]    _zz_22;
  reg        [3:0]    _zz_23;
  wire                _zz_24;
  wire                _zz_25;
  reg                 _zz_26;
  reg        [0:0]    _zz_27;
  reg        [0:0]    _zz_28;

  assign _zz_29 = _zz_1;
  assign _zz_30 = {3'd0, _zz_29};
  assign _zz_31 = _zz_6;
  assign _zz_32 = {2'd0, _zz_31};
  assign _zz_33 = _zz_11;
  assign _zz_34 = {3'd0, _zz_33};
  assign _zz_35 = _zz_16;
  assign _zz_36 = {2'd0, _zz_35};
  assign _zz_37 = _zz_21;
  assign _zz_38 = {3'd0, _zz_37};
  assign _zz_39 = {1'd0, _zz_8};
  assign _zz_40 = _zz_3;
  assign _zz_41 = {1'd0, _zz_18};
  assign _zz_42 = _zz_13;
  always @ (*) begin
    _zz_1 = 1'b0;
    if(ena)begin
      _zz_1 = 1'b1;
    end
  end

  assign _zz_4 = (_zz_3 == 4'b1001);
  assign _zz_5 = (_zz_4 && _zz_1);
  always @ (*) begin
    if(_zz_5)begin
      _zz_2 = 4'b0000;
    end else begin
      _zz_2 = (_zz_3 + _zz_30);
    end
    if(1'b0)begin
      _zz_2 = 4'b0000;
    end
  end

  always @ (*) begin
    _zz_6 = 1'b0;
    if(_zz_5)begin
      _zz_6 = 1'b1;
    end
  end

  assign _zz_9 = (_zz_8 == 3'b101);
  assign _zz_10 = (_zz_9 && _zz_6);
  always @ (*) begin
    if(_zz_10)begin
      _zz_7 = 3'b000;
    end else begin
      _zz_7 = (_zz_8 + _zz_32);
    end
    if(1'b0)begin
      _zz_7 = 3'b000;
    end
  end

  always @ (*) begin
    _zz_11 = 1'b0;
    if(_zz_10)begin
      _zz_11 = 1'b1;
    end
  end

  assign _zz_14 = (_zz_13 == 4'b1001);
  assign _zz_15 = (_zz_14 && _zz_11);
  always @ (*) begin
    if(_zz_15)begin
      _zz_12 = 4'b0000;
    end else begin
      _zz_12 = (_zz_13 + _zz_34);
    end
    if(1'b0)begin
      _zz_12 = 4'b0000;
    end
  end

  always @ (*) begin
    _zz_16 = 1'b0;
    if(_zz_15)begin
      _zz_16 = 1'b1;
    end
  end

  assign _zz_19 = (_zz_18 == 3'b101);
  assign _zz_20 = (_zz_19 && _zz_16);
  always @ (*) begin
    if(_zz_20)begin
      _zz_17 = 3'b000;
    end else begin
      _zz_17 = (_zz_18 + _zz_36);
    end
    if(1'b0)begin
      _zz_17 = 3'b000;
    end
  end

  always @ (*) begin
    _zz_21 = 1'b0;
    if(_zz_20)begin
      _zz_21 = 1'b1;
    end
  end

  assign _zz_24 = (_zz_23 == 4'b1011);
  assign _zz_25 = (_zz_24 && _zz_21);
  always @ (*) begin
    if(_zz_25)begin
      _zz_22 = 4'b0000;
    end else begin
      _zz_22 = (_zz_23 + _zz_38);
    end
    if(1'b0)begin
      _zz_22 = 4'b0000;
    end
  end

  always @ (*) begin
    _zz_26 = 1'b0;
    if(_zz_25)begin
      _zz_26 = 1'b1;
    end
  end

  always @ (*) begin
    _zz_27 = (_zz_28 + _zz_26);
    if(1'b0)begin
      _zz_27 = 1'b0;
    end
  end

  assign ss = {_zz_39,_zz_40};
  assign mm = {_zz_41,_zz_42};
  always @ (*) begin
    case(_zz_23)
      4'b0001 : begin
        hh = {4'd0, _zz_23};
      end
      4'b0010 : begin
        hh = {4'd0, _zz_23};
      end
      4'b0011 : begin
        hh = {4'd0, _zz_23};
      end
      4'b0100 : begin
        hh = {4'd0, _zz_23};
      end
      4'b0101 : begin
        hh = {4'd0, _zz_23};
      end
      4'b0110 : begin
        hh = {4'd0, _zz_23};
      end
      4'b0111 : begin
        hh = {4'd0, _zz_23};
      end
      4'b1000 : begin
        hh = {4'd0, _zz_23};
      end
      4'b1001 : begin
        hh = {4'd0, _zz_23};
      end
      4'b1010 : begin
        hh = 8'h10;
      end
      4'b1011 : begin
        hh = 8'h11;
      end
      4'b0000 : begin
        hh = 8'h12;
      end
      default : begin
        hh = 8'h12;
      end
    endcase
  end

  assign pm = _zz_28;
  always @ (posedge clk) begin
    if(reset) begin
      _zz_3 <= 4'b0000;
      _zz_8 <= 3'b000;
      _zz_13 <= 4'b0000;
      _zz_18 <= 3'b000;
      _zz_23 <= 4'b0000;
      _zz_28 <= 1'b0;
    end else begin
      _zz_3 <= _zz_2;
      _zz_8 <= _zz_7;
      _zz_13 <= _zz_12;
      _zz_18 <= _zz_17;
      _zz_23 <= _zz_22;
      _zz_28 <= _zz_27;
    end
  end


endmodule
