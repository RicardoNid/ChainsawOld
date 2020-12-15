// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : SeqRec
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module SeqRec (
  input               io_data,
  input               io_clear,
  output              io_found,
  input               clk,
  input               reset
);
  wire                seq_0;
  wire                seq_1;
  wire                seq_2;
  wire                seq_3;
  reg        [2:0]    count;

  assign seq_0 = 1'b1;
  assign seq_1 = 1'b1;
  assign seq_2 = 1'b0;
  assign seq_3 = 1'b1;
  assign io_found = (count == 3'b100);
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      count <= 3'b000;
    end else begin
      if(io_clear)begin
        count <= 3'b000;
      end else begin
        if((count == 3'b000))begin
          if((io_data == seq_0))begin
            count <= (count + 3'b001);
          end else begin
            count <= 3'b000;
          end
        end
        if((count == 3'b001))begin
          if((io_data == seq_1))begin
            count <= (count + 3'b001);
          end else begin
            count <= 3'b000;
          end
        end
        if((count == 3'b010))begin
          if((io_data == seq_2))begin
            count <= (count + 3'b001);
          end else begin
            count <= 3'b010;
          end
        end
        if((count == 3'b011))begin
          if((io_data == seq_3))begin
            count <= (count + 3'b001);
          end else begin
            count <= 3'b000;
          end
        end
      end
    end
  end


endmodule
