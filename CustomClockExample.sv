// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : CustomClockExample



module CustomClockExample (
  input               io_clk,
  input               io_aresetn,
  input               io_resetn,
  output     [3:0]    io_result
);
  reg        [3:0]    myArea_myReg = 4'b0111;

  assign io_result = myArea_myReg;
  always @ (posedge io_clk) begin
    myArea_myReg <= (myArea_myReg + 4'b0001);
  end


endmodule
