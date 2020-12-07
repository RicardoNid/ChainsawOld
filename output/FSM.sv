// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : FSM
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815


`define fsm_enumDefinition_binary_sequential_type [1:0]
`define fsm_enumDefinition_binary_sequential_fsm_BOOT 2'b00
`define fsm_enumDefinition_binary_sequential_fsm_stateA 2'b01
`define fsm_enumDefinition_binary_sequential_fsm_stateB 2'b10
`define fsm_enumDefinition_binary_sequential_fsm_stateC 2'b11



`include "ff.sv"
module FSM (
  output reg          io_result,
  input               clk,
  input               reset
);
  wire                fsm_wantExit;
  reg                 fsm_wantStart;
  reg        [7:0]    fsm_counter;
  reg        `fsm_enumDefinition_binary_sequential_type fsm_stateReg;
  reg        `fsm_enumDefinition_binary_sequential_type fsm_stateNext;
  wire                _zz_1;
  wire                _zz_2;
  `ifndef SYNTHESIS
  reg [79:0] fsm_stateReg_string;
  reg [79:0] fsm_stateNext_string;
  `endif


  `ifndef SYNTHESIS
  always @(*) begin
    case(fsm_stateReg)
      `fsm_enumDefinition_binary_sequential_fsm_BOOT : fsm_stateReg_string = "fsm_BOOT  ";
      `fsm_enumDefinition_binary_sequential_fsm_stateA : fsm_stateReg_string = "fsm_stateA";
      `fsm_enumDefinition_binary_sequential_fsm_stateB : fsm_stateReg_string = "fsm_stateB";
      `fsm_enumDefinition_binary_sequential_fsm_stateC : fsm_stateReg_string = "fsm_stateC";
      default : fsm_stateReg_string = "??????????";
    endcase
  end
  always @(*) begin
    case(fsm_stateNext)
      `fsm_enumDefinition_binary_sequential_fsm_BOOT : fsm_stateNext_string = "fsm_BOOT  ";
      `fsm_enumDefinition_binary_sequential_fsm_stateA : fsm_stateNext_string = "fsm_stateA";
      `fsm_enumDefinition_binary_sequential_fsm_stateB : fsm_stateNext_string = "fsm_stateB";
      `fsm_enumDefinition_binary_sequential_fsm_stateC : fsm_stateNext_string = "fsm_stateC";
      default : fsm_stateNext_string = "??????????";
    endcase
  end
  `endif

  assign fsm_wantExit = 1'b0;
  always @ (*) begin
    fsm_wantStart = 1'b0;
    case(fsm_stateReg)
      `fsm_enumDefinition_binary_sequential_fsm_stateA : begin
      end
      `fsm_enumDefinition_binary_sequential_fsm_stateB : begin
      end
      `fsm_enumDefinition_binary_sequential_fsm_stateC : begin
      end
      default : begin
        fsm_wantStart = 1'b1;
      end
    endcase
  end


  ff u_ff (
    // Inputs
    ._zz_1(_zz_1),
    ._zz_2(_zz_2),
    // Outputs
    .io_result(io_result)
  );


  assign _zz_1 = (fsm_stateReg == `fsm_enumDefinition_binary_sequential_fsm_stateB);
  assign _zz_2 = (fsm_stateNext == `fsm_enumDefinition_binary_sequential_fsm_stateB);
  always @ (*) begin
    fsm_stateNext = fsm_stateReg;
    case(fsm_stateReg)
      `fsm_enumDefinition_binary_sequential_fsm_stateA : begin
        fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_stateB;
      end
      `fsm_enumDefinition_binary_sequential_fsm_stateB : begin
        if((fsm_counter == 8'h04))begin
          fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_stateC;
        end
      end
      `fsm_enumDefinition_binary_sequential_fsm_stateC : begin
        fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_stateA;
      end
      default : begin
      end
    endcase
    if(fsm_wantStart)begin
      fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_stateA;
    end
  end

  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      fsm_counter <= 8'h0;
      fsm_stateReg <= `fsm_enumDefinition_binary_sequential_fsm_BOOT;
    end else begin
      fsm_stateReg <= fsm_stateNext;
      case(fsm_stateReg)
        `fsm_enumDefinition_binary_sequential_fsm_stateA : begin
        end
        `fsm_enumDefinition_binary_sequential_fsm_stateB : begin
          fsm_counter <= (fsm_counter + 8'h01);
        end
        `fsm_enumDefinition_binary_sequential_fsm_stateC : begin
        end
        default : begin
        end
      endcase
      if(((! _zz_1) && _zz_2))begin
        fsm_counter <= 8'h0;
      end
    end
  end


endmodule
