// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module top_module (
  input               clk,
  input               reset,
  input               data,
  output              start_shifting
);
  wire                mainCD_seqRec_seq_0;
  wire                mainCD_seqRec_seq_1;
  wire                mainCD_seqRec_seq_2;
  wire                mainCD_seqRec_seq_3;
  wire                mainCD_seqRec_willClear;
  reg                 mainCD_seqRec_recognized;
  reg        [2:0]    mainCD_seqRec_count;

  assign mainCD_seqRec_seq_0 = 1'b1;
  assign mainCD_seqRec_seq_1 = 1'b1;
  assign mainCD_seqRec_seq_2 = 1'b0;
  assign mainCD_seqRec_seq_3 = 1'b1;
  assign mainCD_seqRec_willClear = 1'b0;
  always @ (*) begin
    mainCD_seqRec_recognized = 1'b0;
    mainCD_seqRec_recognized = (mainCD_seqRec_count == 3'b100);
  end

  assign start_shifting = mainCD_seqRec_recognized;
  always @ (posedge clk) begin
    if(reset) begin
      mainCD_seqRec_count <= 3'b000;
    end else begin
      if(mainCD_seqRec_willClear)begin
        mainCD_seqRec_count <= 3'b000;
      end else begin
        if((mainCD_seqRec_count == 3'b000))begin
          if((data == mainCD_seqRec_seq_0))begin
            mainCD_seqRec_count <= (mainCD_seqRec_count + 3'b001);
          end else begin
            mainCD_seqRec_count <= 3'b000;
          end
        end
        if((mainCD_seqRec_count == 3'b001))begin
          if((data == mainCD_seqRec_seq_1))begin
            mainCD_seqRec_count <= (mainCD_seqRec_count + 3'b001);
          end else begin
            mainCD_seqRec_count <= 3'b000;
          end
        end
        if((mainCD_seqRec_count == 3'b010))begin
          if((data == mainCD_seqRec_seq_2))begin
            mainCD_seqRec_count <= (mainCD_seqRec_count + 3'b001);
          end else begin
            mainCD_seqRec_count <= 3'b010;
          end
        end
        if((mainCD_seqRec_count == 3'b011))begin
          if((data == mainCD_seqRec_seq_3))begin
            mainCD_seqRec_count <= (mainCD_seqRec_count + 3'b001);
          end else begin
            mainCD_seqRec_count <= 3'b000;
          end
        end
      end
    end
  end


endmodule
