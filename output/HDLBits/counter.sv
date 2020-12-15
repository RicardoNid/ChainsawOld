// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : top_module
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module top_module (
  input               clk,
  input               reset,
  input               shift_ena,
  input               count_ena,
  input               data,
  output     [3:0]    q
);
  wire       [4:0]    _zz_1;
  reg        [3:0]    mainCD_qReg;

  assign _zz_1 = ({1'd0,mainCD_qReg} <<< 1'b1);
  assign q = mainCD_qReg;
  always @ (posedge clk) begin
    if(shift_ena)begin
      mainCD_qReg <= _zz_1[3:0];
      mainCD_qReg[0] <= data;
    end else begin
      if(count_ena)begin
        mainCD_qReg <= (mainCD_qReg - 4'b0001);
      end
    end
  end


endmodule
