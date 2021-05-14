package Chainsaw

import scala.math._

class RealInfo(val range: AffineForm, val error: Double) {

  def copy = new RealInfo(range.copy, error)

  def errorAdded(error: Double) = {
    require(error > 0)
    new RealInfo(range.copy, this.error + error)
  }

  def upper = range.upper

  def lower = range.lower

  def maxAbs = upper.abs max lower.abs

  def unary_-() = new RealInfo(-range, error)

  def doAddSub(that: RealInfo, add: Boolean) = {
    val range = if (add) this.range + that.range else this.range - that.range
    val error = this.error + that.error
    new RealInfo(range, error)
  }

  def +(that: RealInfo) = doAddSub(that, true)

  def -(that: RealInfo) = doAddSub(that, false)

  def doAddSub(thatConstant: Double, add: Boolean) = {
    val range = if (add) this.range + thatConstant else this.range - thatConstant
    val error = this.error
    new RealInfo(range, error)
  }

  def +(thatConstant: Double) = doAddSub(thatConstant, true)

  def -(thatConstant: Double) = doAddSub(thatConstant, false)

  def *(that: RealInfo) = {
    val range = this.range * that.range
    // this bound is looser than interval arithmetic
    val error = this.error * that.maxAbs +
      that.error * this.maxAbs +
      this.error * that.error
    new RealInfo(range, error)
  }

  def *(thatConstant: Double) = {
    val range = this.range * thatConstant
    val error = this.error * thatConstant
    new RealInfo(range, error)
  }

  def <<(shiftConstant: Int) = {
    val range = this.range * (1 << shiftConstant)
    val error = this.error * (1 << shiftConstant)
    new RealInfo(range, error)
  }

  def >>(shiftConstant: Int) = {
    val scale = pow(2, -shiftConstant)
    val range = this.range * scale
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
