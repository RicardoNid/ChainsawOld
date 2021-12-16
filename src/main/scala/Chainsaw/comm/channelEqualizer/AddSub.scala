package Chainsaw.comm.channelEqualizer

import Chainsaw._
import spinal.core._

case class AddSub(hardType: HardType[SFix]) extends Component {

  val mode = in Bits (2 bits)
  val x, y = in(hardType())
  val ret = out(hardType())

  switch(mode) {
    is(B"00")(ret := -x)
    is(B"01")(ret := x + y)
    is(B"10")(ret := (x >> 1).truncated)
    is(B"11")(ret := (x >> 4).truncated)
  }

  def drive(xOuter: SFix, yOuter: SFix, retOuter: SFix, modeOuter: Bits) = {
    mode := modeOuter
    x := xOuter
    if (yOuter == null) y.assignDontCare() else y := yOuter
    retOuter := ret
  }

  val add: (SFix, SFix, SFix) => Unit = drive(_, _, _, B"01")
  val negate: (SFix, SFix) => Unit = drive(_, null, _, B"00")
  val divide2: (SFix, SFix) => Unit = drive(_, null, _, B"10")
  val divide16: (SFix, SFix) => Unit = drive(_, null, _, B"11")

}

object AddSub {
  def main(args: Array[String]): Unit = {
    VivadoSynth(AddSub(HardType(SFix(7 exp, -10 exp))))
  }
}
