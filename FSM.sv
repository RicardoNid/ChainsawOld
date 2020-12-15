// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : FSM
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5


`define fsm_enumDefinition_binary_sequential_type [0:0]
`define fsm_enumDefinition_binary_sequential_fsm_BOOT 1'b0
`define fsm_enumDefinition_binary_sequential_fsm_S1 1'b1


module FSM (
  output              io_output,
  input               clk,
  input               reset
);
  wire                _zz_1;
  wire                _zz_2;
  wire                fsm_wantExit;
  wire                fsm_wantStart;
  reg                 fsm_timeout_state;
  reg                 fsm_timeout_stateRise;
  wire                fsm_timeout_counter_willIncrement;
  reg                 fsm_timeout_counter_willClear;
  reg        [0:0]    fsm_timeout_counter_valueNext;
  reg        [0:0]    fsm_timeout_counter_value;
  wire                fsm_timeout_counter_willOverflowIfInc;
  wire                fsm_timeout_counter_willOverflow;
  reg        `fsm_enumDefinition_binary_sequential_type fsm_stateReg;
  reg        `fsm_enumDefinition_binary_sequential_type fsm_stateNext;
  `ifndef SYNTHESIS
  reg [63:0] fsm_stateReg_string;
  reg [63:0] fsm_stateNext_string;
  `endif


  assign _zz_1 = ((! (fsm_stateReg == `fsm_enumDefinition_binary_sequential_fsm_BOOT)) && (fsm_stateNext == `fsm_enumDefinition_binary_sequential_fsm_BOOT));
  assign _zz_2 = ((! (fsm_stateReg == `fsm_enumDefinition_binary_sequential_fsm_S1)) && (fsm_stateNext == `fsm_enumDefinition_binary_sequential_fsm_S1));
  `ifndef SYNTHESIS
  always @(*) begin
    case(fsm_stateReg)
      `fsm_enumDefinition_binary_sequential_fsm_BOOT : fsm_stateReg_string = "fsm_BOOT";
      `fsm_enumDefinition_binary_sequential_fsm_S1 : fsm_stateReg_string = "fsm_S1  ";
      default : fsm_stateReg_string = "????????";
    endcase
  end
  always @(*) begin
    case(fsm_stateNext)
      `fsm_enumDefinition_binary_sequential_fsm_BOOT : fsm_stateNext_string = "fsm_BOOT";
      `fsm_enumDefinition_binary_sequential_fsm_S1 : fsm_stateNext_string = "fsm_S1  ";
      default : fsm_stateNext_string = "????????";
    endcase
  end
  `endif

  assign fsm_wantExit = 1'b0;
  assign fsm_wantStart = 1'b0;
  always @ (*) begin
    fsm_timeout_stateRise = 1'b0;
    if(fsm_timeout_counter_willOverflow)begin
      fsm_timeout_stateRise = (! fsm_timeout_state);
    end
    if(_zz_1)begin
      fsm_timeout_stateRise = 1'b0;
    end
    if(_zz_2)begin
      fsm_timeout_stateRise = 1'b0;
    end
  end

  always @ (*) begin
    fsm_timeout_counter_willClear = 1'b0;
    if(_zz_1)begin
      fsm_timeout_counter_willClear = 1'b1;
    end
    if(_zz_2)begin
      fsm_timeout_counter_willClear = 1'b1;
    end
  end

  assign fsm_timeout_counter_willOverflowIfInc = (fsm_timeout_counter_value == 1'b1);
  assign fsm_timeout_counter_willOverflow = (fsm_timeout_counter_willOverflowIfInc && fsm_timeout_counter_willIncrement);
  always @ (*) begin
    fsm_timeout_counter_valueNext = (fsm_timeout_counter_value + fsm_timeout_counter_willIncrement);
    if(fsm_timeout_counter_willClear)begin
      fsm_timeout_counter_valueNext = 1'b0;
    end
  end

  assign fsm_timeout_counter_willIncrement = 1'b1;
  assign io_output = (fsm_stateReg == `fsm_enumDefinition_binary_sequential_fsm_S1);
  always @ (*) begin
    fsm_stateNext = fsm_stateReg;
    case(fsm_stateReg)
      `fsm_enumDefinition_binary_sequential_fsm_S1 : begin
        if(fsm_timeout_state)begin
          fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_BOOT;
        end
      end
      default : begin
        if(fsm_timeout_state)begin
          fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_S1;
        end
      end
    endcase
    if(fsm_wantStart)begin
      fsm_stateNext = `fsm_enumDefinition_binary_sequential_fsm_BOOT;
    end
  end

  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      fsm_timeout_state <= 1'b0;
      fsm_timeout_counter_value <= 1'b0;
      fsm_stateReg <= `fsm_enumDefinition_binary_sequential_fsm_BOOT;
    end else begin
      fsm_timeout_counter_value <= fsm_timeout_counter_valueNext;
      if(fsm_timeout_counter_willOverflow)begin
        fsm_timeout_state <= 1'b1;
      end
      fsm_stateReg <= fsm_stateNext;
      if(_zz_1)begin
        fsm_timeout_state <= 1'b0;
      end
      if(_zz_2)begin
        fsm_timeout_state <= 1'b0;
      end
    end
  end


endmodule
