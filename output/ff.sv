
module ff
  (
    // Inputs
    input  wire _zz_1,
    input  wire _zz_2,
    // Outputs 
    output reg  io_result
  );


  always @ (*) begin
    io_result = 1'b0;
    if((_zz_1 && (! _zz_2)))begin
      io_result = 1'b1;
    end
  end
endmodule : ff
