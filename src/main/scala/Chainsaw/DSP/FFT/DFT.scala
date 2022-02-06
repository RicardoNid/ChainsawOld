package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.math.sqrt

/** implement dft(without decomposition) by "best practice" of given length, including radix2,4,8 and winograd dft
 *
 * @param N         length of dft/idft
 * @param inverse   dft/idft
 * @param dataType  hard type of datapath
 * @param coeffType hard type of coeffs
 * @see TODO: add references
 */

// with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]]

case class DFT(N: Int, inverse: Boolean = false,
               dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component {

  this.setDefinitionName(s"DFT_$N")
  val complexDataType = HardType(ComplexNumber(dataType().maxExp, dataType().minExp))
  val dataIn = in Vec(complexDataType(), N)
  val dataOut = out Vec(complexDataType(), N)

  def delayed[T <: Data](signal: T, pipelined: Boolean) = if (pipelined) RegNext(signal) else signal

  def butterfly(A: ComplexNumber, B: ComplexNumber) = Seq(A + B, A - B)

  def getFftSym(data: Vec[ComplexNumber]) = {
    val N = data.length
    val mid = Seq(data(N / 2))
    val seg0 = data.slice(1, N / 2)
    val seg1 = data.slice(N / 2 + 1, N)
    Vec(Seq(data.head) ++ seg1.reverse ++ mid ++ seg0.reverse)
  }

  val ret: Vec[ComplexNumber] = N match {
    // for 2, 4, 8, use the given box
    case 2 =>
      val Seq(a, b) = dataIn.map(_.d)
      Vec((a + b).d, (a - b).d)

    case 4 =>
      val Seq(a, b, c, d) = dataIn.map(_.d)
      val Seq(e, f, g, h) = Seq((a + c).d, (b + d).d, (a - c).d, (b - d).d)
      val ret = Vec((e + f).d, (g - h.multiplyI).d, (e - f).d, (g + h.multiplyI).d)
      if (!inverse) ret else getFftSym(ret)

    case 8 =>
      val s0 = { // stage 0
        val Seq(a, b, c, d, e, f, g, h) = dataIn.map(_.d)
        Seq(a + e, b + f, c + g, d + h,
          a - e, b - f, c - g, d - h).map(_.d)
      }

      val s1 = { // stage 1
        val Seq(a, b, c, d, e, f, g, h) = s0
        val sqrt2coeff = SF(1 / sqrt(2), coeffType().maxExp exp, coeffType().minExp exp)
        Seq(
          (a + c).d(2), (b + d).d(2),
          (a - c).d(2), (b - d).d(2),
          e.d(2),
          ((f + h).d * sqrt2coeff).d.truncated(dataType),
          g.d(2),
          ((f - h).d * sqrt2coeff).d.truncated(dataType))
      }

      val s2 = { // stage 2
        val Seq(a, b, c, d, e, f, g, h) = s1
        Seq(a + b, a - b,
          c - d.multiplyI, c + d.multiplyI,
          e - f.multiplyI, e + f.multiplyI,
          h - g.multiplyI, h + g.multiplyI).map(_.d)
      }

      val s3 = { // stage 3
        val Seq(a, b, c, d, e, f, g, h) = s2
        Vec(Seq(a, e + g,
          c, e - g,
          b, f - h,
          d, f + h).map(_.d))
      }

      if (!inverse) s3 else getFftSym(s3)
  }

  dataOut := ret

  def latency: Int = N match {
    case 2 => 2
    case 4 => 3
    case 8 => 6
  }
}