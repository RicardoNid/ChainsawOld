package Chainsaw.comm.channelEqualizer

import spinal.core._

case class VecMult(dspType: HardType[SFix], vecSize: Int) extends Component {
  val xs, ys = in(Vec(dspType(), vecSize))
  val rets = out(Vec(dspType(), vecSize))

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) =>
    val trunc = dspType()
    trunc := (x * y).truncated
    ret := RegNext(trunc)
  }

  def drive(xOuter: Vec[SFix], yOuter: Vec[SFix], retOuter: Vec[SFix]) = {
    Seq(xOuter, yOuter).zip(Seq(xs, ys)).foreach { case (signal, port) =>
      if (signal == null) port.assignDontCare() else port := signal
    }
    if (retOuter != null) retOuter := rets
  }

  val mult: (Vec[SFix], Vec[SFix]) => Unit = drive(_, _, null)

  def idle() = drive(null, null, null)

  def init() = {
    Seq(xs, ys).foreach(_.allowOverride)
    idle()
  }

}
