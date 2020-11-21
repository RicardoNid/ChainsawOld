// Generator : SpinalHDL v1.4.2    git head : 804c7bd7b7feaddcc1d25ecef6c208fd5f776f79
// Component : Adder8



module Adder8 (
);
  wire                _zz_1;
  wire                _zz_2;
  wire                _zz_3;
  wire                _zz_4;
  wire                _zz_5;
  wire                _zz_6;
  wire                _zz_7;
  wire                _zz_8;
  wire                _zz_9;
  wire                _zz_10;
  wire                _zz_11;
  wire                _zz_12;
  wire                _zz_13;
  wire                _zz_14;
  wire                _zz_15;
  wire                _zz_16;
  wire                _zz_17;
  wire                _zz_18;
  wire                _zz_19;
  wire                _zz_20;
  wire                _zz_21;
  wire                _zz_22;
  wire                _zz_23;
  wire                _zz_24;
  wire                _zz_25;
  wire                _zz_26;
  wire                _zz_27;
  wire                _zz_28;
  wire                cell0_io_sum;
  wire                cell0_io_cout;
  wire                cell1_io_sum;
  wire                cell1_io_cout;
  wire                cellArray_0_io_sum;
  wire                cellArray_0_io_cout;
  wire                cellArray_1_io_sum;
  wire                cellArray_1_io_cout;
  wire                cellArray_2_io_sum;
  wire                cellArray_2_io_cout;
  wire                cellArray_3_io_sum;
  wire                cellArray_3_io_cout;
  wire                cellArray_4_io_sum;
  wire                cellArray_4_io_cout;
  wire                cellArray_5_io_sum;
  wire                cellArray_5_io_cout;
  wire                cellArray_6_io_sum;
  wire                cellArray_6_io_cout;
  wire                cellArray_7_io_sum;
  wire                cellArray_7_io_cout;

  AdderCell cell0 (
    .io_a       (_zz_1          ), //i
    .io_b       (_zz_2          ), //i
    .io_cin     (_zz_3          ), //i
    .io_sum     (cell0_io_sum   ), //o
    .io_cout    (cell0_io_cout  )  //o
  );
  AdderCell cell1 (
    .io_a       (_zz_4          ), //i
    .io_b       (_zz_5          ), //i
    .io_cin     (cell0_io_cout  ), //i
    .io_sum     (cell1_io_sum   ), //o
    .io_cout    (cell1_io_cout  )  //o
  );
  AdderCell cellArray_0 (
    .io_a       (_zz_6                ), //i
    .io_b       (_zz_7                ), //i
    .io_cin     (_zz_8                ), //i
    .io_sum     (cellArray_0_io_sum   ), //o
    .io_cout    (cellArray_0_io_cout  )  //o
  );
  AdderCell cellArray_1 (
    .io_a       (_zz_9                ), //i
    .io_b       (_zz_10               ), //i
    .io_cin     (cellArray_0_io_cout  ), //i
    .io_sum     (cellArray_1_io_sum   ), //o
    .io_cout    (cellArray_1_io_cout  )  //o
  );
  AdderCell cellArray_2 (
    .io_a       (_zz_11               ), //i
    .io_b       (_zz_12               ), //i
    .io_cin     (_zz_13               ), //i
    .io_sum     (cellArray_2_io_sum   ), //o
    .io_cout    (cellArray_2_io_cout  )  //o
  );
  AdderCell cellArray_3 (
    .io_a       (_zz_14               ), //i
    .io_b       (_zz_15               ), //i
    .io_cin     (_zz_16               ), //i
    .io_sum     (cellArray_3_io_sum   ), //o
    .io_cout    (cellArray_3_io_cout  )  //o
  );
  AdderCell cellArray_4 (
    .io_a       (_zz_17               ), //i
    .io_b       (_zz_18               ), //i
    .io_cin     (_zz_19               ), //i
    .io_sum     (cellArray_4_io_sum   ), //o
    .io_cout    (cellArray_4_io_cout  )  //o
  );
  AdderCell cellArray_5 (
    .io_a       (_zz_20               ), //i
    .io_b       (_zz_21               ), //i
    .io_cin     (_zz_22               ), //i
    .io_sum     (cellArray_5_io_sum   ), //o
    .io_cout    (cellArray_5_io_cout  )  //o
  );
  AdderCell cellArray_6 (
    .io_a       (_zz_23               ), //i
    .io_b       (_zz_24               ), //i
    .io_cin     (_zz_25               ), //i
    .io_sum     (cellArray_6_io_sum   ), //o
    .io_cout    (cellArray_6_io_cout  )  //o
  );
  AdderCell cellArray_7 (
    .io_a       (_zz_26               ), //i
    .io_b       (_zz_27               ), //i
    .io_cin     (_zz_28               ), //i
    .io_sum     (cellArray_7_io_sum   ), //o
    .io_cout    (cellArray_7_io_cout  )  //o
  );

endmodule

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

//AdderCell replaced by AdderCell

module AdderCell (
  input               io_a,
  input               io_b,
  input               io_cin,
  output              io_sum,
  output              io_cout
);

  assign io_sum = ((io_a ^ io_b) ^ io_cin);
  assign io_cout = (((io_a && io_b) || (io_a && io_cin)) || (io_b && io_cin));

endmodule
