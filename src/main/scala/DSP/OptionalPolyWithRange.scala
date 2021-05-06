//package DSP
//
//import DSP.OptionalPoly.undeterminedPoly
//import cc.redberry.rings.poly.multivar.MonomialOrder._
//import cc.redberry.rings.scaladsl.{MultivariateRing, Z}
//
//import scala.collection.JavaConversions._
//
//class OptionalPolyWithRange(field: Field, poly: Poly, val ranges: Array[RealRange]) extends OptionalPoly(field, poly) {
//
//  require(field.variables.length == ranges.length)
//
//  def evaluateTerm(term: Term): RealRange = {
//    val pairs = term.dv().exponents.zip(ranges)
//      .filter { case (i, range) => i > 0 }
//
//    val value = if (pairs.isEmpty)
//      RangeShift(1)
//    else pairs.map { case (i, range) => range ^ i }.reduce(_ * _)
//
//    value * term.coefficient.intValue()
//  }
//
//  def evaluate(poly: Poly, ranges: Array[RealRange]): RealRange = {
//    val terms: Array[Term] = poly.iterator().toArray
//    terms.length match {
//      case 0 => RealRange(0.0, 0.0, this.ranges.head.resolution)
//      case _ => terms.map(evaluateTerm).reduce(_ + _)
//    }
//  }
//
//  def rangesMap = field.variables.zip(ranges).toMap
//
//  /** RealRange of the poly, evaluated from poly and ranges
//   */
//  def range: RealRange = if (undetermined) ranges(0) else evaluate(poly, ranges)
//
//  def unary_-() = {
//    if (undetermined) new OptionalPolyWithRange(this.field, this.poly, Array(-this.ranges.head))
//    else new OptionalPolyWithRange(this.field, this.poly.negate(), this.ranges)
//  }
//
//  def binaryOperation(that: OptionalPolyWithRange,
//                      polyOp: (OptionalPoly, OptionalPoly) => OptionalPoly,
//                      rangeOp: (RealRange, RealRange) => RealRange) = {
//    val newPoly = polyOp(this, that)
//    val newRanges =
//      if (this.undetermined || that.undetermined) // apply the operation on the range
//        Array(rangeOp(this.ranges.head, that.ranges.head))
//      else // merge the ranges independent variables
//        (this.rangesMap ++ that.rangesMap).map(_._2).toArray
//    new OptionalPolyWithRange(newPoly.field, newPoly.poly, newRanges)
//  }
//
//  def +(that: OptionalPolyWithRange) = binaryOperation(that, _ + _, _ + _)
//
//  def -(that: OptionalPolyWithRange) = binaryOperation(that, _ - _, _ - _)
//
//  def *(that: OptionalPolyWithRange) = binaryOperation(that, _ * _, _ * _)
//
//  def binaryOperationOnConstant(constant: Int,
//                                polyOp: (OptionalPoly, Int) => OptionalPoly,
//                                rangeOp: (RealRange, Int) => RealRange): OptionalPolyWithRange = {
//    val newRanges = if (this.undetermined) Array(rangeOp(this.ranges.head, constant))
//    else this.ranges
//    val newPoly = if (this.undetermined) this
//    else polyOp(new OptionalPoly(this.field, this.poly), constant)
//    new OptionalPolyWithRange(newPoly.field, newPoly.poly, newRanges)
//  }
//
//  override def +(constant: Int) = binaryOperationOnConstant(constant, _ + _, _ + _)
//
//  override def -(constant: Int) = binaryOperationOnConstant(constant, _ - _, _ - _)
//
//  override def *(constant: Int) = binaryOperationOnConstant(constant, _ * _, _ * _)
//
//  override def <<(shift: Int) = binaryOperationOnConstant(shift, _ << _, _ << _)
//
//  override def >>(shift: Int) = binaryOperationOnConstant(shift, _ >> _, _ >> _)
//
//  override def toString = s"${range}, InnerPoly: ${expr}"
//}
//
//object OptionalPolyWithRange {
//
//  def apply(name: String, range: RealRange): OptionalPolyWithRange = {
//    val field = new MultivariateRing(Z, Array(name), GREVLEX) // ordinary integer polynomial that we use
//    val poly = field.parse(name)
//    new OptionalPolyWithRange(field, poly, Array(range))
//  }
//
//  /** Factory with no name
//   */
//  def apply(range: RealRange): OptionalPolyWithRange = {
//    new OptionalPolyWithRange(undeterminedPoly.field, undeterminedPoly.poly, Array(range))
//  }
//
//  def main(args: Array[String]): Unit = {
//    val a = OptionalPolyWithRange("a", IntRange(1))
//    val b = OptionalPolyWithRange("b", IntRange(2))
//    val c = OptionalPolyWithRange("c", IntRange(3))
//
//    val aX = OptionalPolyWithRange(IntRange(1))
//    val bX = OptionalPolyWithRange(IntRange(2))
//    val cX = OptionalPolyWithRange(IntRange(3))
//
//    def test(opwr: OptionalPolyWithRange, high: BigDecimal, low: BigDecimal) =
//      assert(opwr.range.upper == high && opwr.range.lower == low, s"yours: ${opwr.expr}, ${opwr.range}")
//
//    test(a + b + c + 4, 10.0, -2.0)
//    test(a * b * c + 2, 8.0, -4.0)
//    test(a * 2 - a, 1.0, -1.0)
//    test(aX * 2 - aX, 3.0, -3.0)
//    test((a + b) * (b + c), 15.0, -11.0) // b^2 \in [0, 4], not [-4,4]
//
//    val input = OptionalPolyWithRange("x", RealRange(1.0, 0.1))
//
//    val mid0 = (input << 2) - input // 3x
//    val mid1 = (input << 3) + input // 9x
//    val mid2 = mid1 + (input << 1) // 11x
//    val mid3 = mid1 + (input << 2) // 13x
//    val mid4 = (mid2 << 4) - mid0 // 173x
//
//    val y1 = mid4 << 1 // 346x
//    val y2 = mid3 << 4 // 208x
//    val y3 = -(mid2 << 2) // -44x
//    val y4 = mid1 // 9x
//
//    val cases = List(mid0, mid1, mid2, mid3, mid4, y1, y2, y3, y4)
//    cases.foreach(println)
//    println(cases.map(_.range.getBitCount).sum)
//
//    val inputX = OptionalPolyWithRange(IntRange(1))
//
//    val mid0X = (inputX << 2) - inputX // 3x
//    val mid1X = (inputX << 3) + inputX // 9x
//    val mid2X = mid1X + (inputX << 1) // 11x
//    val mid3X = mid1X + (inputX << 2) // 13x
//    val mid4X = (mid2X << 4) - mid0X // 173x
//
//    val y1X = mid4X << 1
//    val y2X = mid3X << 4
//    val y3X = -(mid2X << 2)
//    val y4X = mid1X
//
//    val casesX = List(mid0X, mid1X, mid2X, mid3X, mid4X, y1X, y2X, y3X, y4X)
//    casesX.foreach(println)
//    println(casesX.map(_.range.getBitCount).sum)
//
//  }
//}