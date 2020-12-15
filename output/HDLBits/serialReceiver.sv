// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5


`define mainCD_fsm_enumDefinition_binary_sequential_type [2:0]
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT 3'b000
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE 3'b001
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA 3'b010
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH 3'b011
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP 3'b100
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE 3'b101


module top_module (
  input               clk,
  input               reset,
  input               in_,
  output              done
);
  wire                mainCD_fsm_wantExit;
  reg                 mainCD_fsm_wantStart;
  reg        [3:0]    _zz_1;
  reg        `mainCD_fsm_enumDefinition_binary_sequential_type mainCD_fsm_stateReg = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE;
  reg        `mainCD_fsm_enumDefinition_binary_sequential_type mainCD_fsm_stateNext;
  `ifndef SYNTHESIS
  reg [135:0] mainCD_fsm_stateReg_string;
  reg [135:0] mainCD_fsm_stateNext_string;
  `endif


  `ifndef SYNTHESIS
  always @(*) begin
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT : mainCD_fsm_stateReg_string = "mainCD_fsm_BOOT  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE : mainCD_fsm_stateReg_string = "mainCD_fsm_IDLE  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA : mainCD_fsm_stateReg_string = "mainCD_fsm_DATA  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH : mainCD_fsm_stateReg_string = "mainCD_fsm_BRANCH";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP : mainCD_fsm_stateReg_string = "mainCD_fsm_STOP  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE : mainCD_fsm_stateReg_string = "mainCD_fsm_NONE  ";
      default : mainCD_fsm_stateReg_string = "?????????????????";
    endcase
  end
  always @(*) begin
    case(mainCD_fsm_stateNext)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT : mainCD_fsm_stateNext_string = "mainCD_fsm_BOOT  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE : mainCD_fsm_stateNext_string = "mainCD_fsm_IDLE  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA : mainCD_fsm_stateNext_string = "mainCD_fsm_DATA  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH : mainCD_fsm_stateNext_string = "mainCD_fsm_BRANCH";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP : mainCD_fsm_stateNext_string = "mainCD_fsm_STOP  ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE : mainCD_fsm_stateNext_string = "mainCD_fsm_NONE  ";
      default : mainCD_fsm_stateNext_string = "?????????????????";
    endcase
  end
  `endif

  assign mainCD_fsm_wantExit = 1'b0;
  always @ (*) begin
    mainCD_fsm_wantStart = 1'b0;
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE : begin
      end
      default : begin
        mainCD_fsm_wantStart = 1'b1;
      end
    endcase
  end

  assign done = (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP);
  always @ (*) begin
    mainCD_fsm_stateNext = mainCD_fsm_stateReg;
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE : begin
        if((! in_))begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA : begin
        if((_zz_1 <= 4'b0001))begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH : begin
        if(in_)begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP;
        end else begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP : begin
        if(in_)begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE;
        end else begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE : begin
        mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE;
      end
      default : begin
      end
    endcase
    if(mainCD_fsm_wantStart)begin
      mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE;
    end
  end

  always @ (posedge clk) begin
    mainCD_fsm_stateReg <= mainCD_fsm_stateNext;
  end

  always @ (posedge clk) begin
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_IDLE : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA : begin
        _zz_1 <= (_zz_1 - 4'b0001);
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BRANCH : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_STOP : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_NONE : begin
      end
      default : begin
      end
    endcase
    if(((! (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA)) && (mainCD_fsm_stateNext == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DATA)))begin
      _zz_1 <= 4'b1000;
    end
  end


endmodule
