// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : Datatype
// Git hash  : d7943635f0317afaf10af76ed9307a372bbef1fa


`define myState_binary_sequential_type [1:0]
`define myState_binary_sequential_e0 2'b00
`define myState_binary_sequential_e1 2'b01
`define myState_binary_sequential_e2 2'b10
`define myState_binary_sequential_e3 2'b11


module Datatype (
  output     [1:0]    io_output
);

  assign io_output = `myState_binary_sequential_e3;

endmodule
