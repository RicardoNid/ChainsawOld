package projects.FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._

class ComplexNumber(R: SFix, I: SFix) {
  val real = R
  val imag = I

  def +(that: ComplexNumber) = ComplexNumber(real + that.real, imag + that.imag)

  def -(that: ComplexNumber) = ComplexNumber(real - that.real, imag - that.imag)

  // ALGO: 6.10
  def *(that: ComplexNumber) = { // TODO: thick about "static"


    val E = real - imag
    val Z = that.real * E

    val R = ((that.real - that.imag) * imag + Z).truncated
    val I = ((that.real + that.imag) * real - Z).truncated
    ComplexNumber(R, I)
  }
}

object ComplexNumber {

  val zero = data
  zero := 0.0

  def apply(R: SFix = zero, I: SFix= zero) = {
    new ComplexNumber(R, I)
  }

  def apply(R: Double, I: Double) = {
    val real, imag = data
    real := R
    imag := I
    new ComplexNumber(real, imag)
  }
}
