// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : streamTutorial
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815



module streamTutorial (
  input               io_input_valid,
  output              io_input_ready,
  input      [7:0]    io_input_payload,
  output              io_output_valid,
  input               io_output_ready,
  output     [7:0]    io_output_payload,
  input               clk,
  input               reset
);
  wire                _zz_1;
  wire                innerFifo_io_push_ready;
  wire                innerFifo_io_pop_valid;
  wire       [7:0]    innerFifo_io_pop_payload;
  wire       [7:0]    innerFifo_io_occupancy;
  wire       [7:0]    innerFifo_io_availability;

  StreamFifo innerFifo (
    .io_push_valid      (io_input_valid                  ), //i
    .io_push_ready      (innerFifo_io_push_ready         ), //o
    .io_push_payload    (io_input_payload[7:0]           ), //i
    .io_pop_valid       (innerFifo_io_pop_valid          ), //o
    .io_pop_ready       (io_output_ready                 ), //i
    .io_pop_payload     (innerFifo_io_pop_payload[7:0]   ), //o
    .io_flush           (_zz_1                           ), //i
    .io_occupancy       (innerFifo_io_occupancy[7:0]     ), //o
    .io_availability    (innerFifo_io_availability[7:0]  ), //o
    .clk                (clk                             ), //i
    .reset              (reset                           )  //i
  );
  assign io_input_ready = innerFifo_io_push_ready;
  assign io_output_valid = innerFifo_io_pop_valid;
  assign io_output_payload = innerFifo_io_pop_payload;
  assign _zz_1 = 1'b0;

endmodule

module StreamFifo (
  input               io_push_valid,
  output              io_push_ready,
  input      [7:0]    io_push_payload,
  output              io_pop_valid,
  input               io_pop_ready,
  output     [7:0]    io_pop_payload,
  input               io_flush,
  output     [7:0]    io_occupancy,
  output     [7:0]    io_availability,
  input               clk,
  input               reset
);
  reg        [7:0]    _zz_3;
  wire       [0:0]    _zz_4;
  wire       [6:0]    _zz_5;
  wire       [0:0]    _zz_6;
  wire       [6:0]    _zz_7;
  wire       [6:0]    _zz_8;
  wire                _zz_9;
  reg                 _zz_1;
  reg                 logic_pushPtr_willIncrement;
  reg                 logic_pushPtr_willClear;
  reg        [6:0]    logic_pushPtr_valueNext;
  reg        [6:0]    logic_pushPtr_value;
  wire                logic_pushPtr_willOverflowIfInc;
  wire                logic_pushPtr_willOverflow;
  reg                 logic_popPtr_willIncrement;
  reg                 logic_popPtr_willClear;
  reg        [6:0]    logic_popPtr_valueNext;
  reg        [6:0]    logic_popPtr_value;
  wire                logic_popPtr_willOverflowIfInc;
  wire                logic_popPtr_willOverflow;
  wire                logic_ptrMatch;
  reg                 logic_risingOccupancy;
  wire                logic_pushing;
  wire                logic_popping;
  wire                logic_empty;
  wire                logic_full;
  reg                 _zz_2;
  wire       [6:0]    logic_ptrDif;
  reg [7:0] logic_ram [0:127];

  assign _zz_4 = logic_pushPtr_willIncrement;
  assign _zz_5 = {6'd0, _zz_4};
  assign _zz_6 = logic_popPtr_willIncrement;
  assign _zz_7 = {6'd0, _zz_6};
  assign _zz_8 = (logic_popPtr_value - logic_pushPtr_value);
  assign _zz_9 = 1'b1;
  always @ (posedge clk) begin
    if(_zz_9) begin
      _zz_3 <= logic_ram[logic_popPtr_valueNext];
    end
  end

  always @ (posedge clk) begin
    if(_zz_1) begin
      logic_ram[logic_pushPtr_value] <= io_push_payload;
    end
  end

  always @ (*) begin
    _zz_1 = 1'b0;
    if(logic_pushing)begin
      _zz_1 = 1'b1;
    end
  end

  always @ (*) begin
    logic_pushPtr_willIncrement = 1'b0;
    if(logic_pushing)begin
      logic_pushPtr_willIncrement = 1'b1;
    end
  end

  always @ (*) begin
    logic_pushPtr_willClear = 1'b0;
    if(io_flush)begin
      logic_pushPtr_willClear = 1'b1;
    end
  end

  assign logic_pushPtr_willOverflowIfInc = (logic_pushPtr_value == 7'h7f);
  assign logic_pushPtr_willOverflow = (logic_pushPtr_willOverflowIfInc && logic_pushPtr_willIncrement);
  always @ (*) begin
    logic_pushPtr_valueNext = (logic_pushPtr_value + _zz_5);
    if(logic_pushPtr_willClear)begin
      logic_pushPtr_valueNext = 7'h0;
    end
  end

  always @ (*) begin
    logic_popPtr_willIncrement = 1'b0;
    if(logic_popping)begin
      logic_popPtr_willIncrement = 1'b1;
    end
  end

  always @ (*) begin
    logic_popPtr_willClear = 1'b0;
    if(io_flush)begin
      logic_popPtr_willClear = 1'b1;
    end
  end

  assign logic_popPtr_willOverflowIfInc = (logic_popPtr_value == 7'h7f);
  assign logic_popPtr_willOverflow = (logic_popPtr_willOverflowIfInc && logic_popPtr_willIncrement);
  always @ (*) begin
    logic_popPtr_valueNext = (logic_popPtr_value + _zz_7);
    if(logic_popPtr_willClear)begin
      logic_popPtr_valueNext = 7'h0;
    end
  end

  assign logic_ptrMatch = (logic_pushPtr_value == logic_popPtr_value);
  assign logic_pushing = (io_push_valid && io_push_ready);
  assign logic_popping = (io_pop_valid && io_pop_ready);
  assign logic_empty = (logic_ptrMatch && (! logic_risingOccupancy));
  assign logic_full = (logic_ptrMatch && logic_risingOccupancy);
  assign io_push_ready = (! logic_full);
  assign io_pop_valid = ((! logic_empty) && (! (_zz_2 && (! logic_full))));
  assign io_pop_payload = _zz_3;
  assign logic_ptrDif = (logic_pushPtr_value - logic_popPtr_value);
  assign io_occupancy = {(logic_risingOccupancy && logic_ptrMatch),logic_ptrDif};
  assign io_availability = {((! logic_risingOccupancy) && logic_ptrMatch),_zz_8};
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      logic_pushPtr_value <= 7'h0;
      logic_popPtr_value <= 7'h0;
      logic_risingOccupancy <= 1'b0;
      _zz_2 <= 1'b0;
    end else begin
      logic_pushPtr_value <= logic_pushPtr_valueNext;
      logic_popPtr_value <= logic_popPtr_valueNext;
      _zz_2 <= (logic_popPtr_valueNext == logic_pushPtr_value);
      if((logic_pushing != logic_popping))begin
        logic_risingOccupancy <= logic_pushing;
      end
      if(io_flush)begin
        logic_risingOccupancy <= 1'b0;
      end
    end
  end


endmodule
