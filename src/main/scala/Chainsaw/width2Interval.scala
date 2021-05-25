package Chainsaw

import spinal.core._
import scala.math._

object width2Interval {
  def apply(qWidths: QWidths, signed: Boolean = true): RealInfo = {
    val upper =
      pow(2, qWidths.maxExp) - pow(2, qWidths.minExp)
    val lower =
      if (signed) -pow(2, qWidths.maxExp)
      else 0.0
    RealInfo(lower, upper)
  }
}
