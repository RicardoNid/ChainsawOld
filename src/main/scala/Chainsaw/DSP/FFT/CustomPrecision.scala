package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

trait CustomPrecision {

  val dataWidth: Int
  val coeffWidth: Int

  val peak = 6 // TODO: find a better strategy
  val resolution = -(dataWidth - 1 - peak)

  def dataType() = SFix(peak exp, resolution exp)
  def complexDataType =  HardType(ComplexNumber(peak, resolution))

  def coeffType() = SFix(1 exp, -(coeffWidth - 2) exp)
  def coeffDataType() =  ComplexNumber(1, -(coeffWidth - 2))

  def toCoeff: BigDecimal => SFix = SF(_, 1 exp, -(coeffWidth - 2) exp)
}
