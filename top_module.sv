// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815


`define walkingType_binary_sequential_type [1:0]
`define walkingType_binary_sequential_walkingS 2'b00
`define walkingType_binary_sequential_walkRightS 2'b01
`define walkingType_binary_sequential_fallingS 2'b10
`define walkingType_binary_sequential_diggingS 2'b11

`define directionType_binary_sequential_type [0:0]
`define directionType_binary_sequential_leftS 1'b0
`define directionType_binary_sequential_rightS 1'b1


module top_module (
  input               clk,
  input               areset,
  input               bump_left,
  input               bump_right,
  input               ground,
  input               dig,
  output              walk_left,
  output              walk_right,
  output              aaah,
  output              digging
);
  wire                _zz_5;
  reg        `walkingType_binary_sequential_type _zz_1;
  reg        `walkingType_binary_sequential_type _zz_2;
  reg        `directionType_binary_sequential_type _zz_3;
  reg        `directionType_binary_sequential_type _zz_4;
  `ifndef SYNTHESIS
  reg [79:0] _zz_1_string;
  reg [79:0] _zz_2_string;
  reg [47:0] _zz_3_string;
  reg [47:0] _zz_4_string;
  `endif


  assign _zz_5 = (! ground);
  `ifndef SYNTHESIS
  always @(*) begin
    case(_zz_1)
      `walkingType_binary_sequential_walkingS : _zz_1_string = "walkingS  ";
      `walkingType_binary_sequential_walkRightS : _zz_1_string = "walkRightS";
      `walkingType_binary_sequential_fallingS : _zz_1_string = "fallingS  ";
      `walkingType_binary_sequential_diggingS : _zz_1_string = "diggingS  ";
      default : _zz_1_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_2)
      `walkingType_binary_sequential_walkingS : _zz_2_string = "walkingS  ";
      `walkingType_binary_sequential_walkRightS : _zz_2_string = "walkRightS";
      `walkingType_binary_sequential_fallingS : _zz_2_string = "fallingS  ";
      `walkingType_binary_sequential_diggingS : _zz_2_string = "diggingS  ";
      default : _zz_2_string = "??????????";
    endcase
  end
  always @(*) begin
    case(_zz_3)
      `directionType_binary_sequential_leftS : _zz_3_string = "leftS ";
      `directionType_binary_sequential_rightS : _zz_3_string = "rightS";
      default : _zz_3_string = "??????";
    endcase
  end
  always @(*) begin
    case(_zz_4)
      `directionType_binary_sequential_leftS : _zz_4_string = "leftS ";
      `directionType_binary_sequential_rightS : _zz_4_string = "rightS";
      default : _zz_4_string = "??????";
    endcase
  end
  `endif

  always @ (*) begin
    _zz_1 = _zz_2;
    case(_zz_2)
      `walkingType_binary_sequential_walkingS : begin
        if(_zz_5)begin
          _zz_1 = `walkingType_binary_sequential_fallingS;
        end
      end
      `walkingType_binary_sequential_fallingS : begin
        if(ground)begin
          _zz_1 = `walkingType_binary_sequential_walkingS;
        end
      end
      `walkingType_binary_sequential_diggingS : begin
        if((! ground))begin
          _zz_1 = `walkingType_binary_sequential_fallingS;
        end
      end
      default : begin
      end
    endcase
  end

  always @ (*) begin
    _zz_3 = _zz_4;
    case(_zz_4)
      `directionType_binary_sequential_leftS : begin
        if((((ground && (! dig)) && (_zz_2 == `walkingType_binary_sequential_walkingS)) && bump_left))begin
          _zz_3 = `directionType_binary_sequential_rightS;
        end
      end
      default : begin
        if((((ground && (! dig)) && (_zz_2 == `walkingType_binary_sequential_walkingS)) && bump_right))begin
          _zz_3 = `directionType_binary_sequential_leftS;
        end
      end
    endcase
  end

  assign walk_left = ((_zz_2 == `walkingType_binary_sequential_walkingS) && (_zz_4 == `directionType_binary_sequential_leftS));
  assign walk_right = ((_zz_2 == `walkingType_binary_sequential_walkingS) && (_zz_4 == `directionType_binary_sequential_rightS));
  assign aaah = (_zz_2 == `walkingType_binary_sequential_fallingS);
  assign digging = (_zz_2 == `walkingType_binary_sequential_diggingS);
  always @ (posedge clk or posedge areset) begin
    if (areset) begin
      _zz_2 <= `walkingType_binary_sequential_walkingS;
      _zz_4 <= `directionType_binary_sequential_leftS;
    end else begin
      _zz_2 <= _zz_1;
      case(_zz_2)
        `walkingType_binary_sequential_walkingS : begin
          if(! _zz_5) begin
            if(dig)begin
              _zz_2 <= `walkingType_binary_sequential_diggingS;
            end
          end
        end
        default : begin
        end
      endcase
      _zz_4 <= _zz_3;
    end
  end


endmodule
