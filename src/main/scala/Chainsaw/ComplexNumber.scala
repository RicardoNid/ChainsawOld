package Chainsaw

import spinal.core._
import Chainsaw._
import Chainsaw.matlabIO._

// TODO: extend this from Data

/**
 * @param R real part of the complex number
 * @param I imaginary part of the complex number
 */
case class ComplexNumber(peak: Int, resolution: Int) extends Bundle {

  val real = SFix(peak exp, resolution exp)
  val imag = SFix(peak exp, resolution exp)

  def +(that: ComplexNumber): ComplexNumber = ComplexNumber(real + that.real, imag + that.imag)

  def -(that: ComplexNumber): ComplexNumber = ComplexNumber(real - that.real, imag - that.imag)

  def unary_-(): ComplexNumber = ComplexNumber(-real, -imag)

  def multiplyI = ComplexNumber(-imag, real)

  def *(that: SFix) = {
    val R = real * that
    val I = imag * that
    Seq(R, I).foreach(_.addAttribute("use_dsp", "yes"))
    ComplexNumber(R, I)
  }

  def >>(that: Int) = ComplexNumber(real >> that, imag >> that)

  // ALGO: 6.10
  def *(that: ComplexNumber)(implicit pipeline: Int = 0): ComplexNumber = {

    ComplexNumber(this.real * that.real - this.imag * that.imag, this.real * that.imag + this.imag * that.real)

    //    require(pipelined >= 0 && pipelined <= 3)
    // original, directly from algo 6.10
    //        val E = real - imag
    //        val Z = that.real * E
    //        val R = ((that.real - that.imag) * imag + Z).truncated
    //        val I = ((that.real + that.imag) * real - Z).truncated

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
    // stage 2
    doPipeline = pipeline > 0
    val I = D.pipelined - E.pipelined
    val R = E.pipelined + F.pipelined
    Seq(A, B, C).foreach(_.addAttribute("use_dsp", "no"))
    Seq(D, E, F).foreach(_.addAttribute("use_dsp", "yes"))
    Seq(R, I).foreach(_.addAttribute("use_dsp", "no"))
    // final
    doPipeline = pipeline > 1
    ComplexNumber(R.pipelined, I.pipelined)
  }

  def fastMult(that: ComplexNumber, pipeline: Seq[Boolean]) = {

    def delayed0(signal: SFix) = if (pipeline(0)) RegNext(signal) else signal
    def delayed1(signal: SFix) = if (pipeline(1)) RegNext(signal) else signal
    def delayed2(signal: SFix) = if (pipeline(1)) RegNext(signal) else signal

    // stage 0
    val A = that.real + that.imag
    val B = real - imag
    val C = that.real - that.imag
    // stage 1
    val D = A * real
    val E = that.real * B
    val F = imag * C
    Seq(D, E, F).foreach(_.addAttribute("use_dsp", "yes"))
    // stage 2
    val I = (delayed1(D) - delayed1(E)).truncated
    val R = (delayed1(E) + delayed1(F)).truncated
    Seq(I, R).foreach(_.addAttribute("use_dsp", "no"))
    ComplexNumber(delayed2(R), delayed2(I))
  }

  // * i
  // TODO: deprecate after extending ComplexNumber from Data
  def tap: ComplexNumber = ComplexNumber(RegNext(real), RegNext(imag))

  def truncated(dataType: HardType[SFix]) = {
    val retReal, retImag = dataType()
    retReal := real.truncated
    retImag := imag.truncated
    ComplexNumber(retReal, retImag)
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


}

object CN {
  def apply(complex: MComplex, dataType: HardType[SFix]): ComplexNumber = {
    def toSF(value:Double) = SF(value, dataType().maxExp exp, dataType().minExp exp)
    ComplexNumber(toSF(complex.real), toSF(complex.imag))
  }
}
