package spinal.core

import Chainsaw.{RealInfo, printlnWhenDebug, printlnYellow}

import scala.math.{ceil, pow}

/** Factories in this trait would be automatically visible in the project, as the package object extends it
 *
 */
trait RealFactory {
  /** Native factory
   */
  def Real(realInfo: RealInfo, resolution: ExpNumber): Real =
    new Real(realInfo, resolution)

  def RealWithError(realInfo: RealInfo, resolution: ExpNumber): Real = {
    val ret = new Real(realInfo, resolution, withRoundingError = true)
    printlnWhenDebug(s"rounding error ${ret.ulp} introduced")
    ret
  }

  /** The most commonly used factory which initialize a Real from a interval [lower, upper] and a resolution
   */
  def Real(lower: Double, upper: Double, resolution: ExpNumber): Real =
    Real(RealInfo(lower, upper), resolution)

  def RealWithError(lower: Double, upper: Double, resolution: ExpNumber): Real =
    RealWithError(RealInfo(lower, upper), resolution)

  def Real(lower: Double, upper: Double, decimalResolution: Double): Real =
    Real(lower, upper, -log2Up(ceil(1 / decimalResolution).toInt) exp)

  def RealWithError(lower: Double, upper: Double, decimalResolution: Double): Real =
    RealWithError(lower, upper, -log2Up(ceil(1 / decimalResolution).toInt) exp)


  // Integer factories, which will never introduce error
  def UIntReal(upper: Int): Real = Real(0.0, upper, 0 exp)

  def SIntReal(upper: Int): Real = Real(-upper, upper, 0 exp)

  def SIntReal(lower: Int, upper: Int): Real = {
    if (lower >= 0) printlnYellow(s"SIntReal is unnecessary as lower = $lower")
    Real(lower, upper, 0 exp)
  }

  //  https://en.wikipedia.org/wiki/Q_(number_format)
  def QFormatReal(qFormat: QFormat): Real = {
    import qFormat._
    val lower = if (signed) -pow(2, nonFraction - 1) else 0.0
    val upper = (if (signed) pow(2, nonFraction - 1) else pow(2, nonFraction)) - pow(2, -fraction)
    Real(RealInfo(lower, upper), -qFormat.fraction exp)
  }

  def QFormatRealWithError(qFormat: QFormat): Real = {
    import qFormat._
    val lower = if (signed) -pow(2, nonFraction - 1) else 0.0
    val upper = if (signed) pow(2, nonFraction - 1) else pow(2, nonFraction)
    RealWithError(RealInfo(lower, upper), -qFormat.fraction exp)
  }

  /** Constant factory
   */
  def ConstantReal(value: Double, resolution: ExpNumber): Real = {
    val ret = Real(RealInfo(value), resolution)
    ret := value
    ret
  }

  def ConstantRealWithError(value: Double, resolution: ExpNumber): Real = {
    val ret = RealWithError(RealInfo(value), resolution)
    ret := value
    ret
  }
}
