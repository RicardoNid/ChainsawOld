package DSP

import spinal.core.SpinalError

object log2Up {
  def apply(value: BigInt): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    (value - 1).bitLength
  }

  /** Extended the original for real number, give exponent of the nearest upper power of 2
   *
   * Behaves the same on integer as the original one
   */
  def apply(value: Double): Int = {
    if (value < 0) SpinalError(s"No negative value ($value) on ${this.getClass.getSimpleName}")
    scala.math.ceil(scala.math.log(value) / scala.math.log(2.0)).toInt
  }
}
