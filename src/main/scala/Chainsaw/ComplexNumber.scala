package Chainsaw

import spinal.core._

import scala.language.postfixOps


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

  def *(that: ComplexNumber)(implicit config: ComplexMultConfig): ComplexNumber = {
    val mult = ComplexMult(HardType(this), HardType(that))(config)
    mult.a := this
    mult.b := that
    mult.p
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
