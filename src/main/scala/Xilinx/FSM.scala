package Xilinx

import spinal.core._

class FSM extends Component {
  val io = new Bundle {
    val output = out Bits (3 bit)
  }

  val encoding = SpinalEnumEncoding("dynamicEncoding", _ * 2 + 1)

  object stateEnum extends SpinalEnum(encoding) {
    val IDLE, S0, S1, DONE = newElement()
  }

  import stateEnum._

  val stateNext = stateEnum()
  val state = RegNext(stateNext)

  switch(state) {
    is(IDLE) {
      stateNext := S0
    }
    is(S0) {
      stateNext := S1
    }
    is(S1) {
      stateNext := DONE
    }
    is(DONE) {
      stateNext := IDLE
    }
  }

  io.output := state.asBits
}

object FSM {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new FSM)
  }
}
