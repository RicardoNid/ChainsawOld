package Chainsaw.DSP

import Chainsaw.DSP.AlgebraicMode._
import Chainsaw.DSP.CordicArch._
import Chainsaw.DSP.RotationMode._
import Chainsaw.{DSPSimTiming, Double2Fix, Fix2Double, TimingInfo, ChainsawDebug, sameFixedSeq}
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, Flow, master, slave}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import scala.util.Random

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