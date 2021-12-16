package Chainsaw.comm.channelEqualizer

import Chainsaw._
import spinal.core._

import scala.language.postfixOps

case class DSP48Operations() {
  val add = B"01"
  val mult = B"10"
  val mac = B"11"
  val idle = B"00"
}

case class DSP48(dspType: HardType[SFix]) extends Component {

  require(dspType().bitCount == 18)

  val x, y, z = in(dspType())
  val mode = in Bits (2 bits)
  val ret = out(dspType())

  val maxExp = dspType().maxExp
  val minExp = dspType().minExp
  val zero = SF(0, maxExp exp, minExp exp)
  val one = SF(1, maxExp exp, minExp exp)

  switch(mode) { // TODO: merge sub in DSP
    is(B"00")(ret := (x * y).truncated)
    is(B"01")(ret := (x + z))
    is(B"10")(ret := (x - z))
    default(ret := (x + z))
  }

  def drive(modeOuter: Bits, op0: SFix, op1: SFix, op2: SFix, retOuter: SFix): Unit = {
    if (modeOuter == null) mode.assignDontCare() else mode := modeOuter
    Seq(x, y, z).zip(Seq(op0, op1, op2)).foreach { case (port, signal) =>
      if (signal == null) port.assignDontCare()
      else port := signal
    }
    if (retOuter != null) retOuter := ret
  }

  // "instructions"
  val mult: (SFix, SFix, SFix) => Unit = drive(B"00", _, _, null, _)
  val add: (SFix, SFix, SFix) => Unit = drive(B"01", _, null, _, _)
  val sub: (SFix, SFix, SFix) => Unit = drive(B"10", _, null, _, _)

  def idle() = drive(null, null, null, null, null)

}

object DSP48 {
  def main(args: Array[String]): Unit = {
    VivadoSynth(DSP48(HardType(SFix(7 exp, 18 bits))))
  }
}


