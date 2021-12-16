package Chainsaw.comm.channelEqualizer

import spinal.core._

case class VecAddSub(dspType: HardType[SFix], vecSize: Int) extends Component {
  val xs, ys = in(Vec(dspType(), vecSize))
  val mode = in Bits (1 bits)
  //  mode.addAttribute("max_fanout", 32)
  val rets = out(Vec(dspType(), vecSize))

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) => ret := Mux(mode(0), x + y, x - y) }

  def drive(modeOuter: Bits, xOuter: Vec[SFix], yOuter: Vec[SFix], retOuter: Vec[SFix]) = {
    Seq(modeOuter, xOuter, yOuter).zip(Seq(mode, xs, ys)).foreach { case (signal, port) =>
      if (signal == null) port.assignDontCare() else port := signal
    }
    if (retOuter != null) retOuter := rets
  }

  val add: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = drive(B"1", _, _, _)
  val sub: (Vec[SFix], Vec[SFix], Vec[SFix]) => Unit = drive(B"0", _, _, _)

  def idle() = drive(null, null, null, null)

  def init() = {
    Seq(mode, xs, ys).foreach(_.allowOverride)
    idle()
  }
}
