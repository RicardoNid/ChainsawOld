/*
Reference book = Self-Validated Numerical Methods and Applications

The bold parts in ScalaDoc are directly quoted from the book, which are terminologies or routines.

Basically, we use AA to implement interval calculations and IA to implement AA calculations.
That is, for errors, we do not consider the correlations between them, and just accumulate them.
 */

package Chainsaw

import scala.math._

class RealInfo(val interval: AffineForm, val error: Double) {

  override def clone = new RealInfo(interval.clone, error)

  /** Introduce(and accumulate) new error
   *
   */
  def withErrorAdded(error: Double): RealInfo = {
    require(error > 0)
    new RealInfo(interval.clone, this.error + error)
  }

  // expose inner attributes
  def upper = interval.upper
  def lower = interval.lower

  def maxAbs = max(upper.abs, lower.abs)

  def unary_-() = new RealInfo(-interval, error)

  def doAddSub(that: RealInfo, add: Boolean) = {
    val range = if (add) this.interval + that.interval else this.interval - that.interval
    val error = this.error + that.error
    new RealInfo(range, error)
  }

  def +(that: RealInfo) = doAddSub(that, true)

  def -(that: RealInfo) = doAddSub(that, false)

  def doAddSub(thatConstant: Double, add: Boolean) = {
    val range = if (add) this.interval + thatConstant else this.interval - thatConstant
    val error = this.error
    new RealInfo(range, error)
  }

  def +(thatConstant: Double) = doAddSub(thatConstant, true)

  def -(thatConstant: Double) = doAddSub(thatConstant, false)

  def *(that: RealInfo) = {
    val range = this.interval * that.interval
    // this bound is looser than interval arithmetic
    val error = this.error * that.maxAbs +
      that.error * this.maxAbs +
      this.error * that.error
    new RealInfo(range, error)
  }

  def *(thatConstant: Double) = {
    val range = this.interval * thatConstant
    val error = this.error * thatConstant
    new RealInfo(range, error)
  }

  def <<(shiftConstant: Int) = {
    val range = this.interval * (1 << shiftConstant)
    val error = this.error * (1 << shiftConstant)
    new RealInfo(range, error)
  }

  def >>(shiftConstant: Int) = {
    val scale = pow(2, -shiftConstant)
    val range = this.interval * scale
    val error = this.error * scale
    new RealInfo(range, error)
  }

  override def toString: String = s"range: [$lower, $upper], error: $error"
}

object RealInfo {

  def apply(lower: Double, upper: Double)(implicit error: Double = 0.0): RealInfo =
    new RealInfo(AffineForm(lower, upper), error)

  def apply(value: Double) =
    new RealInfo(AffineForm(value), 0.0)

  def main(args: Array[String]): Unit = {
    val x = RealInfo(-1.0, 1.0)(0.01)

    val x5 = (x << 2) + x
    val x39 = (x5 << 3) - x
    val x663 = (x39 << 4) + x39
    val x41623 = (x5 << 13) + x663
  }
}
