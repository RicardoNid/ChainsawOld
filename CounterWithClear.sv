// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : CounterWithClear
// Git hash  : 522eba4ea06e4d798cd2bf77f8088e2ea0ff0815



module CounterWithClear (
  input               io_clear,
  output     [7:0]    io_value,
  input               clk,
  input               reset
);
  reg        [7:0]    register_1;

  assign io_value = register_1;
  always @ (posedge clk or posedge reset) begin
    if (reset) begin
      register_1 <= 8'h0;
    end else begin
      register_1 <= (register_1 + 8'h01);
      if(io_clear)begin
        register_1 <= 8'h0;
      end
    end
  end


endmodule
