package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

trait CustomPrecision {

  val N: Int
  val dataWidth: Int
  val coeffWidth: Int

  val peak = log2Up(N) / 2 // TODO: find a better strategy
  val resolution = -(dataWidth - 1 - peak)

  def dataType() = SFix(peak exp, resolution exp)

  def coeffType() = SFix(1 exp, -(coeffWidth - 2) exp)

  def toCoeff: BigDecimal => SFix = SF(_, 1 exp, -(coeffWidth - 2) exp)
}
