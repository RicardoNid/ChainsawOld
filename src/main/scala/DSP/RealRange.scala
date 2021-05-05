package DSP

import scala.math.{abs, max, min, pow}

/** Descript the possible value of a real number, which is in [A,B](or, [B, A]) and the resolution is R
 *
 * @param valueA One side of the range
 * @param valueB Another side of the range
 * @param valueR Resoulution
 * @example RealRange(-0.1, 0.3, 0.1): A number which should be in {-0.1, 0.0, 0.2, 0.3}
 */
class RealRange(valueA: BigDecimal, valueB: BigDecimal, valueR: BigDecimal) {
  require(valueR >= 0.0)
  val upper = max(valueA.toDouble, valueB.toDouble)
  val lower = min(valueA.toDouble, valueB.toDouble)
  val resolution = valueR.toDouble

  val absPos = if (upper > 0) upper else 0.0 // max abs needed for positive part
  val absNeg = if (lower < 0) -lower else 0.0

  assert(getMinExp <= getMaxExp, "bad resolution, leading to a minExp greater than the maxExp")


  /** Get a new RealRange object with the same attribute
   */
  def copy() = new RealRange(upper, lower, resolution)

  /** Get the bitWidth needed for natural part to have a resoution less or equal than R
   *
   */
  def getMinExp = -log2Up(1.0 / resolution.toDouble)

  /** Get the bitWidth needed for natural part, caution: 2's complment has asymmetric range
   *
   * @example for [-1.5, 2.0], you need 2 bits for the natural part(not including the first bit)
   *
   *          for [-2.0, 1.5], you need only 1 bit
   */
  def getMaxExp = {
    val epsilon = 1E-4 // TODO: not very accurate, but done is better than perfect
    max(log2Up(absPos + pow(2.0, getMinExp)), // for positive part, 4.0 need 3 bits
      log2Up(absNeg)) // while for negative part, -4.0 need only 2 bits
  }

  /** Get the maximum absolute value possible in this RealRange
   *
   * @example for [3.5, 3.6], resolution = 0.1, MinAbs = 3.5
   *
   *          for [-3.5, 3.6], resolution = 0.1, MinAbs = 0.1
   */
  def getMinAbs = {
    if (upper * lower <= 0) resolution // which means that current range contains 0
    else min(abs(upper), abs(lower))
  }

  override def toString: String = s"RealRange: [$lower, $upper], resolution = $resolution"

  def +(that: RealRange) = new RealRange(this.lower + that.lower, this.upper + that.upper, min(this.resolution, that.resolution))

  def -(that: RealRange) = new RealRange(this.lower - that.upper, this.upper - that.lower, min(this.resolution, that.resolution))

  def *(that: RealRange) = {
    val candidates = Array(this.upper * that.upper, this.upper * that.lower, this.lower * that.lower, this.lower * that.upper)
    val newResolution = min(this.getMinAbs * that.resolution, that.getMinAbs * this.resolution)
    new RealRange(candidates.min, candidates.max, newResolution)
  }

  def +(constant: BigDecimal) = new RealRange(this.lower + constant, this.upper + constant, this.resolution + constant)

  def -(constant: BigDecimal) = new RealRange(this.lower - constant, this.upper - constant, this.resolution - constant)

  def *(constant: BigDecimal) = new RealRange(this.lower * constant, this.upper * constant, this.resolution * constant)

  def ^(exponent: Int) = {
    require(exponent > 0)
    Array.fill(exponent)(this).reduce(_ * _)
  }

  def <<(shift: Int) = {
    val scale = pow(2.0, shift)
    new RealRange(this.lower * scale, this.upper * scale, this.resolution * scale)
  }

  def >>(shift: Int) = {
    val scale = pow(2.0, shift)
    new RealRange(this.lower / scale, this.upper / scale, this.resolution / scale)
  }

  /** ShiftLeft with resolution unchanged
   */
  def <<|(shift: Int) = {
    val scale = pow(2.0, shift)
    RealRange(this.lower * scale, this.upper * scale)
  }

  def >>|(shift: Int) = {
    val scale = pow(2.0, shift)
    RealRange(this.lower / scale, this.upper / scale)
  }

  def contains(value: BigDecimal) = upper >= value && lower <= value
}

object RealRange {

  /** Factory for a RealRange with all information
   */
  def apply(valueA: BigDecimal, valueB: BigDecimal, valueR: BigDecimal, name: String): RealRange = new RealRange(valueA, valueB, valueR)

  /** Factory for a RealRange with no variable name
   */
  def apply(valueA: BigDecimal, valueB: BigDecimal, valueR: BigDecimal): RealRange = new RealRange(valueA, valueB, valueR)

  /** Factory for a symmetric RealRange
   */
  def apply(valueA: BigDecimal, valueR: BigDecimal): RealRange = new RealRange(valueA, -valueA, valueR)
}

object IntRange {
  /** Factory for a RealRange of an integer
   */
  def apply(valueA: Int, valueB: Int): RealRange = new RealRange(valueA, valueB, 1.0)

  /** Factory for a symmertic IntRange
   */
  def apply(valueA: Int): RealRange = new RealRange(valueA, -valueA, 1.0)
}