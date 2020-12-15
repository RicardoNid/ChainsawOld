// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5


`define mainCD_fsm_enumDefinition_binary_sequential_type [2:0]
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT 3'b000
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING 3'b001
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING 3'b010
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING 3'b011
`define mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE 3'b100


module top_module (
  input               clk,
  input               reset,
  input               data,
  input               ack,
  output     [3:0]    count,
  output              counting,
  output              done
);
  wire                _zz_2;
  wire       [0:0]    _zz_3;
  wire       [9:0]    _zz_4;
  wire       [4:0]    _zz_5;
  wire                mainCD_fsm_wantExit;
  reg                 mainCD_fsm_wantStart;
  reg        [2:0]    _zz_1;
  wire                mainCD_fsm_seqRec_seq_0;
  wire                mainCD_fsm_seqRec_seq_1;
  wire                mainCD_fsm_seqRec_seq_2;
  wire                mainCD_fsm_seqRec_seq_3;
  reg                 mainCD_fsm_seqRec_willClear;
  reg                 mainCD_fsm_seqRec_willRecognize;
  reg                 mainCD_fsm_seqRec_recognized;
  reg        [2:0]    mainCD_fsm_seqRec_count;
  reg        [3:0]    mainCD_fsm_countReg;
  reg                 mainCD_fsm_count1000_willIncrement;
  wire                mainCD_fsm_count1000_willClear;
  reg        [9:0]    mainCD_fsm_count1000_valueNext;
  reg        [9:0]    mainCD_fsm_count1000_value;
  wire                mainCD_fsm_count1000_willOverflowIfInc;
  wire                mainCD_fsm_count1000_willOverflow;
  reg        `mainCD_fsm_enumDefinition_binary_sequential_type mainCD_fsm_stateReg;
  reg        `mainCD_fsm_enumDefinition_binary_sequential_type mainCD_fsm_stateNext;
  `ifndef SYNTHESIS
  reg [151:0] mainCD_fsm_stateReg_string;
  reg [151:0] mainCD_fsm_stateNext_string;
  `endif


  assign _zz_2 = (mainCD_fsm_count1000_willOverflow && (mainCD_fsm_countReg == 4'b0000));
  assign _zz_3 = mainCD_fsm_count1000_willIncrement;
  assign _zz_4 = {9'd0, _zz_3};
  assign _zz_5 = ({1'd0,mainCD_fsm_countReg} <<< 1);
  `ifndef SYNTHESIS
  always @(*) begin
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT : mainCD_fsm_stateReg_string = "mainCD_fsm_BOOT    ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING : mainCD_fsm_stateReg_string = "mainCD_fsm_WAITING ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING : mainCD_fsm_stateReg_string = "mainCD_fsm_SHIFTING";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING : mainCD_fsm_stateReg_string = "mainCD_fsm_COUNTING";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE : mainCD_fsm_stateReg_string = "mainCD_fsm_DONE    ";
      default : mainCD_fsm_stateReg_string = "???????????????????";
    endcase
  end
  always @(*) begin
    case(mainCD_fsm_stateNext)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_BOOT : mainCD_fsm_stateNext_string = "mainCD_fsm_BOOT    ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING : mainCD_fsm_stateNext_string = "mainCD_fsm_WAITING ";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING : mainCD_fsm_stateNext_string = "mainCD_fsm_SHIFTING";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING : mainCD_fsm_stateNext_string = "mainCD_fsm_COUNTING";
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE : mainCD_fsm_stateNext_string = "mainCD_fsm_DONE    ";
      default : mainCD_fsm_stateNext_string = "???????????????????";
    endcase
  end
  `endif

  assign mainCD_fsm_wantExit = 1'b0;
  always @ (*) begin
    mainCD_fsm_wantStart = 1'b0;
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE : begin
      end
      default : begin
        mainCD_fsm_wantStart = 1'b1;
      end
    endcase
  end

  assign mainCD_fsm_seqRec_seq_0 = 1'b1;
  assign mainCD_fsm_seqRec_seq_1 = 1'b1;
  assign mainCD_fsm_seqRec_seq_2 = 1'b0;
  assign mainCD_fsm_seqRec_seq_3 = 1'b1;
  always @ (*) begin
    mainCD_fsm_seqRec_willClear = 1'b0;
    if(((! (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING)) && (mainCD_fsm_stateNext == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING)))begin
      mainCD_fsm_seqRec_willClear = 1'b1;
    end
  end

  always @ (*) begin
    mainCD_fsm_seqRec_willRecognize = 1'b0;
    mainCD_fsm_seqRec_willRecognize = ((mainCD_fsm_seqRec_count == 3'b011) && (data == mainCD_fsm_seqRec_seq_3));
  end

  always @ (*) begin
    mainCD_fsm_seqRec_recognized = 1'b0;
    mainCD_fsm_seqRec_recognized = (mainCD_fsm_seqRec_count == 3'b100);
  end

  always @ (*) begin
    mainCD_fsm_count1000_willIncrement = 1'b0;
    if((mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING))begin
      mainCD_fsm_count1000_willIncrement = 1'b1;
    end
  end

  assign mainCD_fsm_count1000_willClear = 1'b0;
  assign mainCD_fsm_count1000_willOverflowIfInc = (mainCD_fsm_count1000_value == 10'h3e7);
  assign mainCD_fsm_count1000_willOverflow = (mainCD_fsm_count1000_willOverflowIfInc && mainCD_fsm_count1000_willIncrement);
  always @ (*) begin
    if(mainCD_fsm_count1000_willOverflow)begin
      mainCD_fsm_count1000_valueNext = 10'h0;
    end else begin
      mainCD_fsm_count1000_valueNext = (mainCD_fsm_count1000_value + _zz_4);
    end
    if(mainCD_fsm_count1000_willClear)begin
      mainCD_fsm_count1000_valueNext = 10'h0;
    end
  end

  assign count = mainCD_fsm_countReg;
  assign counting = (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING);
  assign done = (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE);
  always @ (*) begin
    mainCD_fsm_stateNext = mainCD_fsm_stateReg;
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING : begin
        if(mainCD_fsm_seqRec_willRecognize)begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING : begin
        if((_zz_1 <= 3'b001))begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING : begin
        if(_zz_2)begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE;
        end
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE : begin
        if(ack)begin
          mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING;
        end
      end
      default : begin
      end
    endcase
    if(mainCD_fsm_wantStart)begin
      mainCD_fsm_stateNext = `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING;
    end
  end

  always @ (posedge clk) begin
    if(reset) begin
      mainCD_fsm_seqRec_count <= 3'b000;
      mainCD_fsm_countReg <= 4'b0000;
      mainCD_fsm_count1000_value <= 10'h0;
      mainCD_fsm_stateReg <= `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING;
    end else begin
      if(mainCD_fsm_seqRec_willClear)begin
        mainCD_fsm_seqRec_count <= 3'b000;
      end else begin
        if((mainCD_fsm_seqRec_count == 3'b000))begin
          if((data == mainCD_fsm_seqRec_seq_0))begin
            mainCD_fsm_seqRec_count <= (mainCD_fsm_seqRec_count + 3'b001);
          end else begin
            mainCD_fsm_seqRec_count <= 3'b000;
          end
        end
        if((mainCD_fsm_seqRec_count == 3'b001))begin
          if((data == mainCD_fsm_seqRec_seq_1))begin
            mainCD_fsm_seqRec_count <= (mainCD_fsm_seqRec_count + 3'b001);
          end else begin
            mainCD_fsm_seqRec_count <= 3'b000;
          end
        end
        if((mainCD_fsm_seqRec_count == 3'b010))begin
          if((data == mainCD_fsm_seqRec_seq_2))begin
            mainCD_fsm_seqRec_count <= (mainCD_fsm_seqRec_count + 3'b001);
          end else begin
            mainCD_fsm_seqRec_count <= 3'b010;
          end
        end
        if((mainCD_fsm_seqRec_count == 3'b011))begin
          if((data == mainCD_fsm_seqRec_seq_3))begin
            mainCD_fsm_seqRec_count <= (mainCD_fsm_seqRec_count + 3'b001);
          end else begin
            mainCD_fsm_seqRec_count <= 3'b000;
          end
        end
      end
      mainCD_fsm_count1000_value <= mainCD_fsm_count1000_valueNext;
      mainCD_fsm_stateReg <= mainCD_fsm_stateNext;
      case(mainCD_fsm_stateReg)
        `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING : begin
        end
        `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING : begin
          mainCD_fsm_countReg <= _zz_5[3:0];
          mainCD_fsm_countReg[0] <= data;
        end
        `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING : begin
          if(! _zz_2) begin
            if(mainCD_fsm_count1000_willOverflow)begin
              mainCD_fsm_countReg <= (mainCD_fsm_countReg - 4'b0001);
            end
          end
        end
        `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE : begin
        end
        default : begin
        end
      endcase
    end
  end

  always @ (posedge clk) begin
    case(mainCD_fsm_stateReg)
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_WAITING : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING : begin
        _zz_1 <= (_zz_1 - 3'b001);
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_COUNTING : begin
      end
      `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_DONE : begin
      end
      default : begin
      end
    endcase
    if(((! (mainCD_fsm_stateReg == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING)) && (mainCD_fsm_stateNext == `mainCD_fsm_enumDefinition_binary_sequential_mainCD_fsm_SHIFTING)))begin
      _zz_1 <= 3'b100;
    end
  end


endmodule
