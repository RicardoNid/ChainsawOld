package Chainsaw

import spinal.core._
import Chainsaw._
import Chainsaw.matlabIO._

import scala.language.postfixOps

object ProdWidthMode extends Enumeration {
  type ProdWidthMode = Value
  val SAME, PROD = Value
}

import ProdWidthMode._

/** configuration of complex multiplication
 *
 * @param fast       use 3 real multiplications and 5 real additions(rather than 4 & 4)
 * @param pipeline   number of pipelining stages
 * @param resultType hard type of result, this can be utilized to reduce addition cost, it takes the dataType of the product by default
 */
case class ComplexMultConfig(fast: Boolean = true, pipeline: Int = 3, width: ProdWidthMode = SAME) {
  require(pipeline >= 0 && pipeline <= 3)
}

/** complex number type based on SFix
 *
 * @param R real part of the complex number
 * @param I imaginary part of the complex number
 */
case class ComplexNumber(peak: Int, resolution: Int) extends Bundle {

  val real = SFix(peak exp, resolution exp)
  val imag = SFix(peak exp, resolution exp)

  def +(that: ComplexNumber): ComplexNumber = ComplexNumber(real + that.real, imag + that.imag)

  def -(that: ComplexNumber): ComplexNumber = ComplexNumber(real - that.real, imag - that.imag)

  def unary_-(): ComplexNumber = ComplexNumber(-real, -imag)

  // nontrivial computations
  def multiplyI = ComplexNumber(-imag, real)

  def divideI = ComplexNumber(imag, -real)

  def conj = ComplexNumber(real, -imag)

  def *(that: SFix) = {
    val R = real * that
    val I = imag * that
    Seq(R, I).foreach(_.addAttribute("use_dsp", "yes"))
    ComplexNumber(R, I)
  }

  def truncated(dataType: HardType[SFix]) = {
    val retReal, retImag = dataType()
    retReal := real.truncated
    retImag := imag.truncated
    ComplexNumber(retReal, retImag)
  }

  def realType = HardType(this.real)

  def >>(that: Int) = ComplexNumber(real >> that, imag >> that)

  def <<(that: Int) = ComplexNumber(real << that, imag << that)

  // TODO: compare this with Xilinx complex multiplication IP
  def *(that: ComplexNumber)
       (implicit config: ComplexMultConfig): ComplexNumber = {

    ComplexNumber(this.real * that.real - this.imag * that.imag, this.real * that.imag + this.imag * that.real)

    val pipeline = config.pipeline

    implicit class sfixPipeline(sf: SFix) {
      def pipelined(implicit doPipeline: Boolean) = if (doPipeline) RegNext(sf) else sf
    }
    implicit var doPipeline = false
    // stage 0
    val A = that.real +^ that.imag
    val B = real -^ imag
    val C = that.real -^ that.imag
    // stage 1
    doPipeline = pipeline > 2
    val D = A.pipelined * real.pipelined
    val E = B.pipelined * that.real.pipelined
    val F = C.pipelined * imag.pipelined
    Seq(D, E, F).foreach(_.addAttribute("use_dsp", "yes"))
    val Seq(dt, et, ft) = Seq(D, E, F).map{value =>
      if (config.width == SAME) {value.truncated(this.realType)} // this would save the offset of final addition
      else value}
    // stage 2
    doPipeline = pipeline > 0
    val I = dt.pipelined - et.pipelined
    val R = et.pipelined + ft.pipelined
    Seq(I, R).foreach(_.addAttribute("use_dsp", "no")) // or, e would be generated twice and 4 dsps would be consumed

    // final
    doPipeline = pipeline > 1
    ComplexNumber(R.pipelined, I.pipelined)
  }
}

object ComplexNumber {

  def apply(R: SFix, I: SFix): ComplexNumber = {
    require(R.maxExp == I.maxExp && R.minExp == I.minExp)
    val peak = R.maxExp
    val resolution = R.minExp
    val ret = new ComplexNumber(peak, resolution)
    ret.real := R
    ret.imag := I
    ret
  }

  def apply(R: Double, I: Double, dataType: HardType[SFix]): ComplexNumber = {
    val real, imag = dataType()
    real := R
    imag := I
    ComplexNumber(real, imag)
  }

  def apply(dataType: HardType[SFix]): ComplexNumber = {
    ComplexNumber(dataType().maxExp, dataType().minExp)
  }
}

object CN {
  def apply(complex: BComplex, dataType: HardType[SFix]): ComplexNumber = {
    def toSF(value: Double) = SF(value, dataType().maxExp exp, dataType().minExp exp)

    ComplexNumber(toSF(complex.real), toSF(complex.imag))
  }
}
