package Chainsaw

import spinal.core._

import scala.math.{ceil, log, max, pow}

case class QWidths(maxExp: Int, minExp: Int)

object interval2Width {
  def apply(realInfo: RealInfo, resolution: ExpNumber): QWidths = {
    val ulp = pow(2, resolution.value)

    def log2(value: Double) = log(value) / log(2.0)

    def log2Up(value: Double) = ceil(log2(value)).toInt

    /** Bits need for a bound regardless of its signedness
      */
    def bitsForBound(bound: Double) = if (bound >= 0.0) log2Up(bound + ulp) else log2Up(-bound)

    val maxExp = max(bitsForBound(realInfo.upper), bitsForBound(realInfo.lower))
    val minExp = resolution.value
    QWidths(maxExp, minExp)
  }
}
