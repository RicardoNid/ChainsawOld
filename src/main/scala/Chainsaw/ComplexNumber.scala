package Chainsaw

import spinal.core._
import Chainsaw._

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

  // TODO: verify whether 0 - imag has bad effect or not
  def multiplyI = ComplexNumber(imag.getZero - imag, real)

  def *(that: SFix) = {
    val R = real * that
    val I = imag * that
    Seq(R, I).foreach(_.addAttribute("use_dsp", "yes"))
    ComplexNumber(R, I)
  }

  // ALGO: 6.10
  def *(that: ComplexNumber)(implicit pipelined: Boolean = false): ComplexNumber = {
    // original, directly from algo 6.10
    //        val E = real - imag
    //        val Z = that.real * E
    //        val R = ((that.real - that.imag) * imag + Z).truncated
    //        val I = ((that.real + that.imag) * real - Z).truncated
    //    def delayed(signal: SFix) = if (pipelined) RegNext(signal) else signal

    def delayed[T <: Data](signal: T) = if (pipelined) RegNext(signal) else signal


    // improved, using more variables for pipelining
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
    val I = (delayed(D) - delayed(E)).truncated
    val R = (delayed(E) + delayed(F)).truncated
    Seq(I, R).foreach(_.addAttribute("use_dsp", "no"))
    val R1 = if (pipelined) RegNext(R) else R
    val I1 = if (pipelined) RegNext(I) else I

    ComplexNumber(R1, I1)
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
}

object ComplexNumber {

  private val zero = globalType
  zero := 0.0

  def apply(R: SFix, I: SFix): ComplexNumber = {
    require(R.maxExp == I.maxExp && R.minExp == I.minExp)
    val peak = R.maxExp
    val resolution = R.minExp
    val ret = new ComplexNumber(peak, resolution)
    ret.real := R
    ret.imag := I
    ret
  }

  def apply(R: Double, I: Double): ComplexNumber = {
    val real, imag = globalType
    real := R
    imag := I
    ComplexNumber(real, imag)
  }
}
