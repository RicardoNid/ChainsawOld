package Chainsaw.DSP.DAS

import spinal.core._
import spinal.lib._
import Para._

case class Unwrap() extends Component {
  val io = new Bundle {
    val xn, xn1 = in(dataType())
    val xnOut   = out(dataType())
  }

  val m  = io.xn1.raw(peakExp + resolutionExp downto resolutionExp)
  val v0 = io.xn1.raw.resize(resolutionExp).asUInt

  val n  = io.xn.raw(peakExp + resolutionExp downto resolutionExp)
  val v1 = io.xn.raw.resize(resolutionExp).asUInt

  val a = SInt(peakExp + 1 bits)
  val b = UInt(resolutionExp bits)
  b := v1

  val one = dataType()
  one := 1

  val mAddOrSumOne = Mux(v0 > v1, m + 1, m - 1)
  when(io.xn + one <= io.xn1 || io.xn >= io.xn1 + one) {
    when(m.lsb ^ n.lsb) {
      a := mAddOrSumOne
    } otherwise {
      a := m
    }
  } otherwise {
    a := n
  }

  io.xnOut.assignFromBits(a ## b)
}
