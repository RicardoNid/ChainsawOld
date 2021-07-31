package Chainsaw.DSP

import Chainsaw.{TimingInfo, _}
import spinal.core._

case class CordicSimData(x: Double, y: Double, z: Double) {
  override def toString: String = s"x: $x, y: $y, z: $z"
}

case class CordicData() extends Bundle {
  val x: SFix = SFix(1 exp, -14 exp) // 1QN
  val y: SFix = SFix(1 exp, -14 exp) // 1QN
  val z: SFix = SFix(2 exp, -13 exp) // 2QN
}


class CORDICDUT(cordicConfig: CordicConfig) extends DSPDUTTiming[CordicData, CordicData]{
  override val input = in(CordicData())
  val cordic: CORDIC = CORDIC(input.x, input.y, input.z, cordicConfig)
  override val output = out(CordicData())
  output.x := cordic._1.truncated
  output.y := cordic._2.truncated
  output.z := cordic._3.truncated
  override val timing: TimingInfo = cordic.getTimingInfo
}