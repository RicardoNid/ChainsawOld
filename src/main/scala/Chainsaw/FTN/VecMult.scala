package Chainsaw.FTN

import spinal.core._

case class VecMult(vecType: HardType[Vec[SFix]]) extends Component {
  val xs, ys = in(vecType())
  val rets = out(vecType())

  rets.zip(xs.zip(ys)).foreach { case (ret, (x, y)) =>
    val trunc = cloneOf(vecType().head)
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
