package DSP

import DSP.OptionalPoly.undeterminedPoly
import cc.redberry.rings.poly.multivar.MonomialOrder._
import cc.redberry.rings.scaladsl.{MultivariateRing, Z}

import scala.collection.JavaConversions._

class OptionalPolyWithRange(field: Field, poly: Poly, val ranges: Array[RealRange]) extends OptionalPoly(field, poly) {

  require(field.variables.length == ranges.length)

  def evaluateTerm(term: Term): RealRange = {
    val pairs = term.dv().exponents.zip(ranges)
      .filter { case (i, range) => i > 0 }

    val value = if (pairs.isEmpty)
      RangeShift(1)
    else pairs.map { case (i, range) => range ^ i }.reduce(_ * _)

    value * term.coefficient.intValue()
  }

  def evaluate(poly: Poly, ranges: Array[RealRange]): RealRange = {
    val terms: Array[Term] = poly.iterator().toArray
    terms.length match {
      case 0 => RealRange(0.0, 0.0, this.ranges.head.resolution)
      case _ => terms.map(evaluateTerm).reduce(_ + _)
    }
  }

  def rangesMap = field.variables.zip(ranges).toMap

  /** RealRange of the poly, evaluated from poly and ranges
   */
  def range: RealRange = if (undetermined) ranges(0) else evaluate(poly, ranges)

  def binaryOperation(that: OptionalPolyWithRange,
                      realPolyOp: (OptionalPoly, OptionalPoly) => OptionalPoly,
                      realRangeOp: (RealRange, RealRange) => RealRange) = {
    val newPoly = realPolyOp(this, that)
    val newRanges =
      if (this.undetermined || that.undetermined) // apply the operation on the range
        Array(realRangeOp(this.ranges.head, that.ranges.head))
      else // merge the ranges independent variables
        (this.rangesMap ++ that.rangesMap).map(_._2).toArray
    new OptionalPolyWithRange(newPoly.field, newPoly.poly, newRanges)
  }

  def +(that: OptionalPolyWithRange) = binaryOperation(that, _ + _, _ + _)

  def -(that: OptionalPolyWithRange) = binaryOperation(that, _ - _, _ - _)

  def *(that: OptionalPolyWithRange) = binaryOperation(that, _ * _, _ * _)

  //  def binaryOperationOnConstant(constant: Int,
  //                                realPolyOp: (OptionalPoly, Int) => OptionalPoly,
  //                                realRangeOp: (RealRange, Int) => RealRange): OptionalPoly = {
  //    val newPoly = realPolyOp(this, constant)
  //    val newRanges = if (this.undetermined) Array(realRangeOp(this.ranges.head, constant))
  ////    else this.ranges
  //  }

}

object OptionalPolyWithRange {

  def apply(name: String, range: RealRange): OptionalPolyWithRange = {
    val field = new MultivariateRing(Z, Array(name), GREVLEX) // ordinary integer polynomial that we use
    val poly = field.parse(name)
    new OptionalPolyWithRange(field, poly, Array(range))
  }

  def apply(range: RealRange): OptionalPolyWithRange = {
    new OptionalPolyWithRange(undeterminedPoly.field, undeterminedPoly.poly, Array(range))
  }

  def main(args: Array[String]): Unit = {
    val x = OptionalPolyWithRange("x", IntRange(1))
    println(x.range)
    println(x.expr)

    val y = OptionalPolyWithRange("y", IntRange(2, -1))
    val z = x - x
    println(z.range)
    println(z.expr)

    val unknownX = OptionalPolyWithRange(IntRange(0, 1))
    val unknownZ = unknownX - unknownX
    println(unknownZ.range)
    println(unknownZ.expr)

    val field = new MultivariateRing(Z, Array("x", "y", "z"), GREVLEX)
    val poly = field.parse("x^2 + y*x + 3*z + 5")

    val complex = new OptionalPolyWithRange(field, poly, Array(
      IntRange(1),
      IntRange(2),
      IntRange(3)))
    println(complex.range)
    println(complex.range.getMaxExp)
    println(complex.range.getMinExp)
    println(complex.expr)

  }
}