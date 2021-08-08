package Chainsaw

import spinal.core._
import Chainsaw._

// TODO: extend this from Data

/**
 * @param R real part of the complex number
 * @param I imaginary part of the complex number
 */
case class ComplexNumber(R: SFix, I: SFix) extends Bundle {
  val real: SFix = R
  val imag: SFix = I

  def +(that: ComplexNumber): ComplexNumber = ComplexNumber(real + that.real, imag + that.imag)

  def -(that: ComplexNumber): ComplexNumber = ComplexNumber(real - that.real, imag - that.imag)

  def unary_-():ComplexNumber = ComplexNumber(-real, -imag)

  // TODO: verify whether 0 - imag has bad effect or not
  def multiplyI = ComplexNumber(imag.getZero - imag, real)

  // ALGO: 6.10
  def *(that: ComplexNumber)(implicit pipelined: Boolean = false): ComplexNumber = {
    // original, directly from algo 6.10
    //        val E = real - imag
    //        val Z = that.real * E
    //        val R = ((that.real - that.imag) * imag + Z).truncated
    //        val I = ((that.real + that.imag) * real - Z).truncated

    // improved, using more variables for pipelining
    // stage 0
    val A = that.real + that.imag
    val B = real - imag
    val C = that.real - that.imag
    // stage 1
    val D = A * real
    val E = that.real * B
    val F = imag * C

    //    println(s"complex multiplication invoke")
    //    println(A.getBitsWidth, real.getBitsWidth)
    //    println(that.real.getBitsWidth, B.getBitsWidth)
    //    println(imag.getBitsWidth, C.getBitsWidth)
    //    Seq(D, E, F).foreach(_.addAttribute("use_dsp", "yes"))
    val delayedDEF = if (pipelined) Seq(D, E, F).map(RegNext(_)) else Seq(D, E, F) // retiming
    val D1 = delayedDEF(0)
    val E1 = delayedDEF(1)
    val F1 = delayedDEF(2)
    // stage 2
    val I = (D1 - E1).truncated
    val R = (E1 + F1).truncated
    val R1 = if (pipelined) RegNext(R) else R
    val I1 = if (pipelined) RegNext(I) else I

    ComplexNumber(R1, I1)
  }

  // * i


  // TODO: deprecate after extending ComplexNumber from Data
  def tap: ComplexNumber = ComplexNumber(RegNext(real), RegNext(imag))
}

object ComplexNumber {

  private val zero = globalType
  zero := 0.0

  def apply(R: Double, I: Double) = {
    val real, imag = globalType
    real := R
    imag := I
    new ComplexNumber(real, imag)
  }
}
