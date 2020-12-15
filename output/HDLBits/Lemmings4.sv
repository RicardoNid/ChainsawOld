// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5


`define mainCD_fsm_enumDefinition_binary_sequential_type [2:0]
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT 3'b000
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK 3'b001
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG 3'b010
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL 3'b011
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DEAD 3'b100


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
  wire                _zz_1;
  wire       [0:0]    _zz_2;
  wire       [4:0]    _zz_3;
  wire                mainCD_fsm_wantExit;
  reg                 mainCD_fsm_wantStart;
  reg                 mainCD_fsm_fallTimeout_state;
  reg                 mainCD_fsm_fallTimeout_stateRise;
  wire                mainCD_fsm_fallTimeout_counter_willIncrement;
  reg                 mainCD_fsm_fallTimeout_counter_willClear;
  reg        [4:0]    mainCD_fsm_fallTimeout_counter_valueNext;
  reg        [4:0]    mainCD_fsm_fallTimeout_counter_value;
  wire                mainCD_fsm_fallTimeout_counter_willOverflowIfInc;
  wire                mainCD_fsm_fallTimeout_counter_willOverflow;
  reg                 mainCD_fsm_directionLeft;
  wire                mainCD_fsm_listen;
  reg        `mainCD_fsm_enumDefinition_binary_sequential_type mainCD_fsm_stateReg;
  reg        `mainCD_fsm_enumDefinition_binary_sequential_type mainCD_fsm_stateNext;
  `ifndef SYNTHESIS
  reg [119:0] mainCD_fsm_stateReg_string;
  reg [119:0] mainCD_fsm_stateNext_string;
  `endif


  assign _zz_1 = ((! (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL)) && (mainCD_fsm_stateNext == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL));
  assign _zz_2 = mainCD_fsm_fallTimeout_counter_willIncrement;
  assign _zz_3 = {4'd0, _zz_2};
  `ifndef SYNTHESIS
  always @(*) begin
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT : mainCD_fsm_stateReg_string = "mainCD_fsm_BOOT";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK : mainCD_fsm_stateReg_string = "mainCD_fsm_WALK";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG : mainCD_fsm_stateReg_string = "mainCD_fsm_DIG ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL : mainCD_fsm_stateReg_string = "mainCD_fsm_FALL";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DEAD : mainCD_fsm_stateReg_string = "mainCD_fsm_DEAD";
      default : mainCD_fsm_stateReg_string = "???????????????";
    endcase
  end
  always @(*) begin
    case(mainCD_fsm_stateNext)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT : mainCD_fsm_stateNext_string = "mainCD_fsm_BOOT";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK : mainCD_fsm_stateNext_string = "mainCD_fsm_WALK";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG : mainCD_fsm_stateNext_string = "mainCD_fsm_DIG ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL : mainCD_fsm_stateNext_string = "mainCD_fsm_FALL";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DEAD : mainCD_fsm_stateNext_string = "mainCD_fsm_DEAD";
      default : mainCD_fsm_stateNext_string = "???????????????";
    endcase
  end
  `endif

  assign mainCD_fsm_wantExit = 1'b0;
  always @ (*) begin
    mainCD_fsm_wantStart = 1'b0;
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DEAD : begin
      end
      default : begin
        mainCD_fsm_wantStart = 1'b1;
      end
    endcase
  end

  always @ (*) begin
    mainCD_fsm_fallTimeout_stateRise = 1'b0;
    if(mainCD_fsm_fallTimeout_counter_willOverflow)begin
      mainCD_fsm_fallTimeout_stateRise = (! mainCD_fsm_fallTimeout_state);
    end
    if(_zz_1)begin
      mainCD_fsm_fallTimeout_stateRise = 1'b0;
    end
  end

  always @ (*) begin
    mainCD_fsm_fallTimeout_counter_willClear = 1'b0;
    if(_zz_1)begin
      mainCD_fsm_fallTimeout_counter_willClear = 1'b1;
    end
  end

  assign mainCD_fsm_fallTimeout_counter_willOverflowIfInc = (mainCD_fsm_fallTimeout_counter_value == 5'h13);
  assign mainCD_fsm_fallTimeout_counter_willOverflow = (mainCD_fsm_fallTimeout_counter_willOverflowIfInc && mainCD_fsm_fallTimeout_counter_willIncrement);
  always @ (*) begin
    if(mainCD_fsm_fallTimeout_counter_willOverflow)begin
      mainCD_fsm_fallTimeout_counter_valueNext = 5'h0;
    end else begin
      mainCD_fsm_fallTimeout_counter_valueNext = (mainCD_fsm_fallTimeout_counter_value + _zz_3);
    end
    if(mainCD_fsm_fallTimeout_counter_willClear)begin
      mainCD_fsm_fallTimeout_counter_valueNext = 5'h0;
    end
  end

  assign mainCD_fsm_fallTimeout_counter_willIncrement = 1'b1;
  assign mainCD_fsm_listen = ((ground && (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK)) && (! dig));
  assign walk_left = ((mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK) && mainCD_fsm_directionLeft);
  assign walk_right = ((mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK) && (! mainCD_fsm_directionLeft));
  assign aaah = (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL);
  assign digging = (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG);
  always @ (*) begin
    mainCD_fsm_stateNext = mainCD_fsm_stateReg;
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK : begin
        if((! ground))begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL;
        end else begin
          if(dig)begin
            mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG;
          end
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DIG : begin
        if((! ground))begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_FALL : begin
        if(ground)begin
          if(mainCD_fsm_fallTimeout_state)begin
            mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DEAD;
          end else begin
            mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK;
          end
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DEAD : begin
      end
      default : begin
      end
    endcase
    if(mainCD_fsm_wantStart)begin
      mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK;
    end
  end

  always @ (posedge clk or posedge areset) begin
    if (areset) begin
      mainCD_fsm_fallTimeout_state <= 1'b0;
      mainCD_fsm_fallTimeout_counter_value <= 5'h0;
      mainCD_fsm_directionLeft <= 1'b1;
      mainCD_fsm_stateReg <= `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WALK;
    end else begin
      mainCD_fsm_fallTimeout_counter_value <= mainCD_fsm_fallTimeout_counter_valueNext;
      if(mainCD_fsm_fallTimeout_counter_willOverflow)begin
        mainCD_fsm_fallTimeout_state <= 1'b1;
      end
      if(((mainCD_fsm_listen && (! mainCD_fsm_directionLeft)) && bump_right))begin
        mainCD_fsm_directionLeft <= 1'b1;
      end
      if(((mainCD_fsm_listen && mainCD_fsm_directionLeft) && bump_left))begin
        mainCD_fsm_directionLeft <= 1'b0;
      end
      mainCD_fsm_stateReg <= mainCD_fsm_stateNext;
      if(_zz_1)begin
        mainCD_fsm_fallTimeout_state <= 1'b0;
      end
    end
  end


endmodule
