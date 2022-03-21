package myNTT

import spinal.core._

class switch[T <: Data](dataType: HardType[T]) extends Component {

  val dataIn  = Vec(in(dataType()), 2)
  val dataOut = Vec(out(dataType()), 2)
  val switch  = in(Bool())

  dataOut(0) := dataIn(1)
  dataOut(1) := dataIn(0)
  when(switch) {
    dataOut(0) := dataIn(0)
    dataOut(1) := dataIn(1)
  }
}

object switch {
  def apply[T <: Data](in0: T, in1: T, sw: T): (T, T) = {
    val switcher = new switch(in0)
    switcher.dataIn(0) := in0
    switcher.dataIn(1) := in1
    switcher.switch    := sw.asBits.lsb
    (switcher.dataOut(0), switcher.dataOut(1))
  }
}
