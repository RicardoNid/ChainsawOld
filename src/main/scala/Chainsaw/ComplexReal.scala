package Chainsaw

import spinal.core._

/** A simple Hardware Complex Number type
  * @param real
  *   A Real as real part
  * @param imag
  *   A Real as imaginary part
  */
class ComplexReal(val real: Real, val imag: Real) {

  def +(that: ComplexReal) = new ComplexReal(this.real + that.real, this.imag + that.imag)

  def -(that: ComplexReal) = new ComplexReal(this.real - that.real, this.imag - that.imag)

  def *(that: ComplexReal): ComplexReal = {
    val C      = that.real
    val CplusS = that.real + that.imag
    val CsubS  = that.real - that.imag
    val E      = this.real - this.imag
    val Z      = E * C
    val R      = this.imag * CsubS + Z
    val I      = this.real * CplusS - Z
    new ComplexReal(R, I)
  }

}

object ComplexReal {
  def apply(real: Real, imag: Real): ComplexReal = new ComplexReal(real, imag)
}
