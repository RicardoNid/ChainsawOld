// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : FSM


`define stateEnum_dynamicEncoding_type [2:0]
`define stateEnum_dynamicEncoding_IDLE 3'b001
`define stateEnum_dynamicEncoding_S0 3'b011
`define stateEnum_dynamicEncoding_S1 3'b101
`define stateEnum_dynamicEncoding_DONE 3'b111


module FSM (
  output     [2:0]    io_output,
  input               clk,
  input               reset
);
  reg        `stateEnum_dynamicEncoding_type stateNext;
  reg        `stateEnum_dynamicEncoding_type state;
  `ifndef SYNTHESIS
  reg [31:0] stateNext_string;
  reg [31:0] state_string;
  `endif


  `ifndef SYNTHESIS
  always @(*) begin
    case(stateNext)
      `stateEnum_dynamicEncoding_IDLE : stateNext_string = "IDLE";
      `stateEnum_dynamicEncoding_S0 : stateNext_string = "S0  ";
      `stateEnum_dynamicEncoding_S1 : stateNext_string = "S1  ";
      `stateEnum_dynamicEncoding_DONE : stateNext_string = "DONE";
      default : stateNext_string = "????";
    endcase
  end
  always @(*) begin
    case(state)
      `stateEnum_dynamicEncoding_IDLE : state_string = "IDLE";
      `stateEnum_dynamicEncoding_S0 : state_string = "S0  ";
      `stateEnum_dynamicEncoding_S1 : state_string = "S1  ";
      `stateEnum_dynamicEncoding_DONE : state_string = "DONE";
      default : state_string = "????";
    endcase
  end
  `endif

  always @ (*) begin
    case(state)
      `stateEnum_dynamicEncoding_IDLE : begin
        stateNext = `stateEnum_dynamicEncoding_S0;
      end
      `stateEnum_dynamicEncoding_S0 : begin
        stateNext = `stateEnum_dynamicEncoding_S1;
      end
      `stateEnum_dynamicEncoding_S1 : begin
        stateNext = `stateEnum_dynamicEncoding_DONE;
      end
      default : begin
        stateNext = `stateEnum_dynamicEncoding_IDLE;
      end
    endcase
  end

  assign io_output = state;
  always @ (posedge clk) begin
    state <= stateNext;
  end


endmodule
