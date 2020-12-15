// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : ComplexDataType
// Git hash  : dc2e7e237db31ab3a9b8a328557ea33d950732f5



module ComplexDataType (
  input      [31:0]   io_input_inst,
  output reg [31:0]   io_output_inst,
  input      [31:0]   io_anotherInput,
  output reg [31:0]   io_anotherOutput
);

  always @ (*) begin
    io_output_inst = io_input_inst;
    io_output_inst[31 : 26] = (~ io_input_inst[31 : 26]);
  end

  always @ (*) begin
    io_anotherOutput = io_anotherInput;
    if(((io_anotherInput & 32'hfe00707f) == 32'h00000033))begin
      io_anotherOutput[11 : 7] = (~ io_anotherInput[11 : 7]);
    end
  end


endmodule
