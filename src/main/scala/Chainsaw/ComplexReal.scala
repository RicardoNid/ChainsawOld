package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class ComplexReal(val real: Real, val imag: Real) {

  def +(that:ComplexReal) = new ComplexReal(this.real + that.real, this.imag + that.imag)

  def -(that:ComplexReal) = new ComplexReal(this.real - that.real, this.imag - that.imag)

  def *(that:ComplexReal) = {
    val C = that.real
    val CplusS = that.real + that.imag
    val CsubS = that.real - that.imag
    val E = this.real - this.imag
    val Z = C * E
    val R = CsubS * this.imag + Z
    val I = CplusS * this.real - Z
    new ComplexReal(R, I)
  }

}

object ComplexReal {
  def apply(real: Real, imag: Real): ComplexReal = new ComplexReal(real, imag)
}
