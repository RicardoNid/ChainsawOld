package Chainsaw

import spinal.core.{ExpNumber, QFormat, log2Up, _}

import scala.math.ceil

/** Factories in this trait would be automatically visible in the project, as the package object extends it
  */
trait RealFactory {

  //  Two basic factories, determine the signal from the interval/QWidths

  /** Basic factory that starts from the interval
    */
  def Real(realInfo: RealInfo, resolution: ExpNumber): Real = {
    val qWidths = interval2Width(realInfo, resolution)
    if (qWidths.maxExp > 24 || qWidths.minExp < -25)
      printlnYellow(
        "currently, because of the limitation of double-precision backend, " +
          "a correct maxExp expansion is not promised when you have [minExp, maxExp] outside of [-25, 24]" +
          "that is, " +
          "an upper bound pow(2.0, 25) with a resolution pow(2.0 -25) may lead to " +
          "25 integer bits, rather than 26, which contains the upper bound"
      )
    new Real(realInfo, qWidths)
  }

  /** Basic factory that starts from the QWidths
    */
  def Real(qWidths: QWidths, signed: Boolean = true): Real = {
    val realInfo = width2Interval(qWidths, signed)
    new Real(realInfo, qWidths)
  }

  //  User-oriented factories that start from the interval

  /** Lower + upper + resolution
    */
  def Real(lower: Double, upper: Double, resolution: ExpNumber): Real =
    Real(RealInfo(lower, upper), resolution)

  /** Lower + upper + resolution
    */
  def Real(lower: Double, upper: Double, decimalResolution: Double): Real =
    Real(lower, upper, -log2Up(ceil(1 / decimalResolution).toInt) exp)

  /** Lower + upper + resolution for integer
    */
  def IntReal(lower: Double, upper: Double): Real =
    Real(lower, upper, 0 exp)

  /** Fixed-like API
    */
  // User-oriented factories that start from the QWidths
  def Real(maxExp: ExpNumber, minExp: ExpNumber): Real = {
    Real(QWidths(maxExp.value, minExp.value))
  }

  /** UInt-like API
    */
  def UIntReal(bits: BitCount): Real =
    Real(QWidths(bits.value, 0), signed = false)

  /** SInt-like API
    */
  def SIntReal(bits: BitCount): Real =
    Real(QWidths(bits.value, 0), signed = true)

  /** Q-Format API
    *
    * @see
    *   [[https://en.wikipedia.org/wiki/Q_(number_format) Q format]]
    */
  def QFormatReal(qFormat: QFormat): Real = {
    import qFormat._
    val maxExp = if (signed) nonFraction - 1 else nonFraction
    val minExp = -fraction
    Real(QWidths(maxExp, minExp), signed = signed)
  }
}

object R {

  /** Real literal factory, the constant has not only a type but also an initial connection
    */
  def apply(value: Double, resolution: ExpNumber): Real = {
    val ret = Real(RealInfo(value), resolution)
    ret := value
    ret
  }
}
