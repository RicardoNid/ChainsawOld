// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815


`define StateMachineEnum_binary_sequential_type [1:0]
`define StateMachineEnum_binary_sequential_e0 2'b00
`define StateMachineEnum_binary_sequential_e1 2'b01
`define StateMachineEnum_binary_sequential_e2 2'b10


module top_module (
  input               clk,
  input               areset,
  input               bump_left,
  input               bump_right,
  output reg          walk_left,
  output reg          walk_right
);
  reg                 _zz_1;
  reg        `StateMachineEnum_binary_sequential_type _zz_2;
  reg        `StateMachineEnum_binary_sequential_type _zz_3;
  `ifndef SYNTHESIS
  reg [15:0] _zz_2_string;
  reg [15:0] _zz_3_string;
  `endif


  `ifndef SYNTHESIS
  always @(*) begin
    case(_zz_2)
      `StateMachineEnum_binary_sequential_e0 : _zz_2_string = "e0";
      `StateMachineEnum_binary_sequential_e1 : _zz_2_string = "e1";
      `StateMachineEnum_binary_sequential_e2 : _zz_2_string = "e2";
      default : _zz_2_string = "??";
    endcase
  end
  always @(*) begin
    case(_zz_3)
      `StateMachineEnum_binary_sequential_e0 : _zz_3_string = "e0";
      `StateMachineEnum_binary_sequential_e1 : _zz_3_string = "e1";
      `StateMachineEnum_binary_sequential_e2 : _zz_3_string = "e2";
      default : _zz_3_string = "??";
    endcase
  end
  `endif

  always @ (*) begin
    _zz_1 = 1'b0;
    case(_zz_2)
      `StateMachineEnum_binary_sequential_e1 : begin
      end
      `StateMachineEnum_binary_sequential_e2 : begin
      end
      default : begin
        _zz_1 = 1'b1;
      end
    endcase
  end

  always @ (*) begin
    walk_left = 1'b1;
    case(_zz_2)
      `StateMachineEnum_binary_sequential_e1 : begin
        walk_left = 1'b1;
      end
      `StateMachineEnum_binary_sequential_e2 : begin
        walk_left = 1'b0;
      end
      default : begin
      end
    endcase
  end

  always @ (*) begin
    walk_right = 1'b0;
    case(_zz_2)
      `StateMachineEnum_binary_sequential_e1 : begin
        walk_right = 1'b0;
      end
      `StateMachineEnum_binary_sequential_e2 : begin
        walk_right = 1'b1;
      end
      default : begin
      end
    endcase
  end

  always @ (*) begin
    _zz_3 = _zz_2;
    case(_zz_2)
      `StateMachineEnum_binary_sequential_e1 : begin
        if(bump_left)begin
          _zz_3 = `StateMachineEnum_binary_sequential_e2;
        end
      end
      `StateMachineEnum_binary_sequential_e2 : begin
        if(bump_right)begin
          _zz_3 = `StateMachineEnum_binary_sequential_e1;
        end
      end
      default : begin
      end
    endcase
    if(_zz_1)begin
      _zz_3 = `StateMachineEnum_binary_sequential_e1;
    end
  end

  always @ (posedge clk or posedge areset) begin
    if (areset) begin
      _zz_2 <= `StateMachineEnum_binary_sequential_e0;
    end else begin
      _zz_2 <= _zz_3;
    end
  end


endmodule
