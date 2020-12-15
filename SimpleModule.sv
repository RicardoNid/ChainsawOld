// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : SimpleModule
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module SimpleModule (
  input               io_input,
  output              io_output,
  input               clk,
  input               reset
);
  reg                 outputReg;

  assign io_output = outputReg;
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      outputReg <= 1'b0;
    end else begin
      outputReg <= ((! io_input) || io_input);
    end
  end


endmodule
