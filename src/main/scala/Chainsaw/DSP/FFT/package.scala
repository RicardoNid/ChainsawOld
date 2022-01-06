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

import scala.util.{Failure, Success, Try}
import scala.math.Pi

package object FFT {
  val eng = AsyncEng.get()

  def WNnk(N: Int, nk: Int): BComplex = exp(BComplex(0, -2 * Pi * nk / N))

  def isPowR(input: Int, radix: Int): Boolean = if (input == radix) true else isPowR(input / radix, radix)

  /** optimize the multiplication with coeffs of fourier transform
   */
  def multiplyWNnk(signal: ComplexNumber, index: Int, N: Int,
                   dataType: HardType[SFix], coeffType: HardType[SFix]): ComplexNumber = {

    implicit class sfixPipeline[T <: Data](sf: T) {
      def pipelined(implicit doPipeline: Boolean) = if (doPipeline) RegNext(sf) else sf
    }
    implicit var doPipeline = false

    val latency = cmultConfig.pipeline
    def delayed(signal: ComplexNumber) = Delay(signal, latency)

    def toFixedCoeff: Double => SFix = SF(_, coeffType().maxExp exp, coeffType().minExp exp)

    // multiply (1 - j) / sqrt(2)
    def multiply1minusj(signal: ComplexNumber) = {
      val A = signal.real + signal.imag
      val B = signal.imag - signal.real
      doPipeline = latency > 1
      val fullReal = A.pipelined * toFixedCoeff(1 / scala.math.sqrt(2.0))
      val fullImag = B.pipelined * toFixedCoeff(1 / scala.math.sqrt(2.0))
      doPipeline = latency > 0
      val fullComplex = ComplexNumber(fullReal.pipelined, fullImag.pipelined)
      doPipeline = latency > 2
      fullComplex.truncated(dataType).pipelined
    }

    val trivialValue = if (N % 8 == 0 && index % (N / 8) == 0) index / (N / 8) else -1
    trivialValue match { // nontrivial values
      case 0 => delayed(signal)
      case 2 => delayed(-signal.multiplyI)
      case 4 => delayed(-signal)
      case 6 => delayed(signal.multiplyI)

      case 1 => multiply1minusj(signal)
      case 3 => multiply1minusj(-signal.multiplyI)
      case 5 => multiply1minusj(-signal)
      case 7 => multiply1minusj(signal.multiplyI)

      case _ => { // trivial values
        val coeffValue = WNnk(N, index)
        val coeff = ComplexNumber(toFixedCoeff(coeffValue.real), toFixedCoeff(coeffValue.imag))
        val fullComplex = signal * coeff
        fullComplex.truncated(dataType)
      }
    }
  }
}
