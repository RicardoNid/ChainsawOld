package Chainsaw

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._

class width2IntervalTest extends AnyFunSuite {

  test("testTrivialWidths2Interval") {
    assert(width2Interval(QWidths(3, -3)).upper == 8 - 0.125)
    assert(width2Interval(QWidths(3, -3), false).upper == 8 - 0.125)
    assert(width2Interval(QWidths(3, -3)).lower == -8)
    assert(width2Interval(QWidths(3, -3), false).lower == 0.0)
  }

  // make sure that for a full interval, widths2Interval and interval2Widths are reversible
  test("testWidths2Interval2Widths") {
    val temp = QWidths(1, -3)
    def reversible(qWidths: QWidths) = {
      val info      = width2Interval(qWidths, ChainsawRand.nextBoolean())
      val recovered = interval2Width(info, qWidths.minExp exp)
      assert(recovered.minExp == qWidths.minExp)
      assert(recovered.maxExp == qWidths.maxExp)
    }
    (0 until 10000)
      .map(_ => QWidths(ChainsawRand.nextInt(24), -ChainsawRand.nextInt(25)))
      .foreach(reversible)
  }
}
