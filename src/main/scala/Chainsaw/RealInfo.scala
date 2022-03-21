/*
Reference book = Self-Validated Numerical Methods and Applications

The bold parts in ScalaDoc are directly quoted from the book, which are terminologies or routines.

Basically, we use AA to implement interval calculations and IA to implement AA calculations.
That is, for errors, we do not consider the correlations between them, and just accumulate them.

The operations off this class are designed for digital IC behaviors,
  however, this class only focus on the interval and error of a signal, on how they are
    introduced
    propagated
  as for the word length strategy, they are implemented in specific DataTypes
 */

package Chainsaw

import scala.math._

/** Numeric information of a signal
  *
  * @param interval
  *   the interval of a signal, which is [lower, upper]
  * @param error
  *   error >= 0.0
  *
  * intervals has nothing to do with errors, while errors are sometimes dependent on intervals
  * @example
  *   new error introduced through a multiplication is dependent on intervals
  */
class RealInfo(val interval: AffineForm, var error: Double) {

  require(error >= 0.0)
  override def clone = new RealInfo(interval.clone, error)

  /** Introduce(and accumulate) new error
    */
  def withErrorAdded(error: Double): RealInfo = {
    require(error > 0)
    new RealInfo(interval.clone, this.error + error)
  }

  // expose inner attributes on intervals and errors
  def upper      = interval.upper
  def lower      = interval.lower
  def isConstant = interval.isConstant
  def constant   = interval.constant

  def maxAbs = max(upper.abs, lower.abs)

  // operations
  // for the interval part, we use affine arithmetic
  // TODO: for the error part, we use interval arithmetic
  def unary_-() = new RealInfo(-interval, error)

  /** +/-
    */
  def doAddSub(that: RealInfo, add: Boolean): RealInfo = {
    val interval = if (add) this.interval + that.interval else this.interval - that.interval
    println(s"when it comes to RealInfo ${interval.intervalTerms.keySet.mkString(" ")}")
    val error = this.error + that.error
    new RealInfo(interval, error)
  }
  def +(that: RealInfo): RealInfo = doAddSub(that, add = true)
  def -(that: RealInfo): RealInfo = doAddSub(that, add = false)

  /** +/- with constant
    */
  def doAddSub(thatConstant: Double, add: Boolean): RealInfo = {
    val interval = if (add) this.interval + thatConstant else this.interval - thatConstant
    val error    = this.error
    new RealInfo(interval, error)
  }
  def +(thatConstant: Double): RealInfo = doAddSub(thatConstant, add = true)
  def -(thatConstant: Double): RealInfo = doAddSub(thatConstant, add = false)

  def *(that: RealInfo): RealInfo = {
    val interval = this.interval * that.interval
    // TODO: this bound is looser than interval arithmetic, improve it
    val error = this.error * that.maxAbs +
      that.error * this.maxAbs +
      this.error * that.error
    println(s"error after multipication: $error")
    new RealInfo(interval, error)
  }

  def *(thatConstant: Double): RealInfo = {
    val interval = this.interval * thatConstant
    val error    = this.error * thatConstant
    new RealInfo(interval, error)
  }

  def <<(shiftConstant: Int): RealInfo = *(pow(2, shiftConstant))
  def >>(shiftConstant: Int): RealInfo = *(pow(2, -shiftConstant))

  override def toString: String = s"interval: [$lower, $upper], error: $error"
}

object RealInfo {

  // In factories, error should be a secondary parameter which will seldomly introduced by factories
  /** Native factory
    */
  def apply(interval: AffineForm, error: ErrorNumber): RealInfo =
    new RealInfo(interval, error.value)

  def apply(value: Double) =
    new RealInfo(AffineForm(value), 0.0)

  def apply(value: Double, error: ErrorNumber) =
    new RealInfo(AffineForm(value), error.value)

  /** The most commonly used factory which initialize an affine form from a interval [lower, upper]
    */
  def apply(lower: Double, upper: Double): RealInfo =
    new RealInfo(AffineForm(lower, upper), 0.0)

  def apply(lower: Double, upper: Double, error: ErrorNumber): RealInfo =
    new RealInfo(AffineForm(lower, upper), error.value)

  def main(args: Array[String]): Unit = {
    val x = RealInfo(-1.0, 1.0, 0.01 err)

    val x5     = (x   << 2) + x
    val x39    = (x5  << 3) - x
    val x663   = (x39 << 4) + x39
    val x41623 = (x5  << 13) + x663

    println(x41623)
    println(x41623.interval)
  }
}
