package Chainsaw

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

import scala.math.pow

class interval2WidthTest extends AnyFunSuite {

  test("testMaxExpExpansion") {
    val matrix = Array.tabulate(50, 50) { case (max, min) =>
      val maxValue = max - 25
      val minValue = min - 25
      if (maxValue >= minValue)
        assert(
          interval2Width(RealInfo(0.0, pow(2.0, maxValue)), minValue exp).maxExp == maxValue + 1,
          s"bad pair maxExp: $maxValue, minExp: $minValue, the maxExp is not expanded "
        )
    }
  }

}
