package Chainsaw

import spinal.core._

// TODO: extend this from Data
/**
 * @param R real part of the complex number
 * @param I imaginary part of the complex number
 */
class ComplexNumber(R: SFix, I: SFix) extends Bundle {
  val real: SFix = R
  val imag: SFix = I

  def +(that: ComplexNumber): ComplexNumber = ComplexNumber(real + that.real, imag + that.imag)

  def -(that: ComplexNumber): ComplexNumber = ComplexNumber(real - that.real, imag - that.imag)

  // ALGO: 6.10
  def *(that: ComplexNumber): ComplexNumber = {
    val E = real - imag
    val Z = that.real * E
    val R = ((that.real - that.imag) * imag + Z).truncated
    val I = ((that.real + that.imag) * real - Z).truncated
    ComplexNumber(R, I)
  }

  // TODO: deprecate after extending ComplexNumber from Data
  def tap: ComplexNumber = ComplexNumber(RegNext(real), RegNext(imag))
}

object ComplexNumber {

  private val zero = globalType
  zero := 0.0

  def apply(): Unit = {

  }

  def apply(R: SFix = zero, I: SFix = zero): ComplexNumber = {
    new ComplexNumber(R, I)
  }

  def apply(R: Double, I: Double) = {
    val real, imag = globalType
    real := R
    imag := I
    new ComplexNumber(real, imag)
  }
}
