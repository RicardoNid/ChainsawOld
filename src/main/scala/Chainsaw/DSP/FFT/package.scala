package Chainsaw.DSP

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import scala.math.Pi

package object FFT {
  val eng = AsyncEng.get()

  def WNnk(N: Int, nk: Int): BComplex = exp(BComplex(0, -2 * Pi * nk / N))

  def isPowR(input: Int, radix: Int): Boolean = if (input == radix) true else isPowR(input / radix, radix)

  /** optimize the multiplication with coeffs of fourier transform
   */
  def multiplyWNnk(signal: ComplexNumber, coeffType: HardType[SFix],
                   index: Int, N: Int)
                  (implicit complexMultConfig: ComplexMultConfig): ComplexNumber = {

    val dataType = signal.realType
    val latency = complexMultConfig.pipeline

    def toFixedCoeff: Double => SFix = SF(_, coeffType().maxExp exp, coeffType().minExp exp)

    /** 90 degree counterclockwise
     *
     * @param quadrant number of 90 degree
     */
    def toQuadrant(signal: ComplexNumber, quadrant: Int) = {
      quadrant match {
        case 0 => signal.d
        case 1 => (-signal.multiplyI).d
        case 2 => (-signal).d
        case 3 => signal.multiplyI.d
      }
    }

    /** multiply (1 - j) / sqrt(2), i.e. 45 degree counterclockwise
     */
    def multiply1minusj(signal: ComplexNumber) = {
      val sqrt2Factor = toFixedCoeff(1 / scala.math.sqrt(2.0)) // * factor = / sqrt2
      val usePREG = if (complexMultConfig.pipeline - 3 > 0) 1 else 0
      import signal._
      ComplexNumber(
        ((real +^ imag).d * sqrt2Factor).d,
        ((imag -^ real).d * sqrt2Factor).d
      ).d(usePREG)
    }

    var positiveIndex = index
    while (positiveIndex < 0) positiveIndex += N
    val caseValue = if (N % 8 == 0 && positiveIndex % (N / 8) == 0) positiveIndex / (N / 8) else -1

    if (caseValue != -1) { // special factors
      val quadrant = caseValue / 2
      val mode = caseValue % 2

      val mode0Balancing = latency - 1
      val mode1Balancing = latency - 4

      if (mode == 0) toQuadrant(signal, quadrant).d(mode0Balancing)
      else multiply1minusj(toQuadrant(signal, quadrant))
        .truncated(dataType)
        .d(if (mode1Balancing < 0) 0 else mode1Balancing)
    } else {
      val coeffValue = WNnk(N, index)
      val coeff = ComplexNumber(toFixedCoeff(coeffValue.real), toFixedCoeff(coeffValue.imag))
      val fullComplex = signal * coeff
      fullComplex.truncated(dataType)
    }
  }
}
