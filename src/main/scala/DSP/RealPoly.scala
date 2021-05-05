package DSP

import DSP.RealPoly.undeterminedPoly
import cc.redberry.rings
import cc.redberry.rings.poly.multivar.MonomialOrder._
import cc.redberry.rings.poly.multivar._
import cc.redberry.rings.scaladsl.{IntZ, MultivariateRing, Z, _}

import scala.collection.JavaConversions._

class RealPoly(val field: Field, val poly: Poly) {

  val expr = poly.toString(field.variables: _*)

  val undetermined = field.variables.contains("none")

  def toUnionField(that: RealPoly) = {
    val unionField = new MultivariateRing(Z, this.field.variables.union(that.field.variables).distinct, GREVLEX)
    (unionField, unionField.parse(this.expr), unionField.parse(that.expr))
  }

  def binaryOperation(that: RealPoly, operator: (Field, Poly, Poly) => Poly) = {
    if (this.undetermined || that.undetermined) undeterminedPoly
    else {
      val (unionfield, newThis, newThat) = toUnionField(that)
      new RealPoly(unionfield, operator(unionfield, newThis, newThat))
    }
  }

  def binaryOperationOnConstant(constant: Int, operator: (Field, Poly, IntZ) => Poly) = {
    if (this.undetermined) undeterminedPoly
    else new RealPoly(field, operator(field, poly, Z(constant)))
  }

  def +(that: RealPoly) = binaryOperation(that, _.add(_, _))

  def -(that: RealPoly) = binaryOperation(that, _.subtract(_, _))

  def *(that: RealPoly) = binaryOperation(that, _.multiply(_, _))

  def +(constant: Int) = binaryOperationOnConstant(constant, _.addConstant(_, _))

  def -(constant: Int) = binaryOperationOnConstant(constant, _.subtractConstant(_, _))

  def *(constant: Int) = binaryOperationOnConstant(constant, _.multiplyConstant(_, _))

  def <<(shift: Int) = binaryOperationOnConstant(1 << shift, _.multiplyConstant(_, _))

  def >>(shift: Int) = undeterminedPoly
}

object RealPoly {

  /** The extrance of a signal, which is always represented by a independent variable
   */
  def apply(name: String): RealPoly = { // the name should not contain any arithmetic operator
    val field = new MultivariateRing(Z, Array(name), GREVLEX) // ordinary integer polynomial that we use
    val poly = field.parse(name)
    new RealPoly(field, poly)
  }

  def undeterminedPoly = RealPoly("none")

  def main(args: Array[String]): Unit = {

    val input = RealPoly("x")
    val mid0 = (input << 2) - input // 3x
    val mid1 = (input << 3) + input // 9x
    val mid2 = mid1 + (input << 1) // 11x
    val mid3 = mid1 + (input << 2) // 13x
    val mid4 = (mid2 << 4) - mid0 // 173x

    List(mid0, mid1, mid2, mid3, mid4).foreach(poly => println(poly.expr))

    val temp0 = RealPoly("a")
    val temp1 = RealPoly("b")
    val result = (temp0 * 3 + temp1) * (temp0 + temp1 * 2)
    val terms = result.poly.iterator().toArray.map(_.dv().exponents)
    println(terms.map(_.mkString(" ")).mkString("\n"))

    val anoterResult = (temp0 * 3 + temp1)
  }
}

class RealPolyWithRange(field: Field, poly: Poly, val ranges: Array[RealRange]) extends RealPoly(field, poly) {

  require(field.variables.length == ranges.length)

  def evaluateTerm(term: Term): RealRange = {
    val temp = term.dv().exponents.zip(ranges)
      .filter { case (i, range) => i > 0 }
      .map { case (i, range) => range ^ i }
      .reduce(_ * _)
    temp * term.coefficient.intValue()
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

  def binaryOperation(that: RealPolyWithRange, realPolyOp: (RealPoly, RealPoly) => RealPoly, realRangeOp: (RealRange, RealRange) => RealRange) = {
    val newPoly = realPolyOp(this, that)
    val newRanges =
      if (this.undetermined || that.undetermined) // apply the operation on the range
        Array(realRangeOp(this.ranges.head, that.ranges.head))
      else // merge the ranges independent variables
        (this.rangesMap ++ that.rangesMap).map(_._2).toArray
    new RealPolyWithRange(newPoly.field, newPoly.poly, newRanges)
  }

  def +(that: RealPolyWithRange) = binaryOperation(that, _ + _, _ + _)

  def -(that: RealPolyWithRange) = binaryOperation(that, _ - _, _ - _)

  def *(that: RealPolyWithRange) = binaryOperation(that, _ * _, _ * _)

}

object RealPolyWithRange {

  def apply(name: String, range: RealRange): RealPolyWithRange = {
    val field = new MultivariateRing(Z, Array(name), GREVLEX) // ordinary integer polynomial that we use
    val poly = field.parse(name)
    new RealPolyWithRange(field, poly, Array(range))
  }

  def apply(range: RealRange): RealPolyWithRange = {
    new RealPolyWithRange(undeterminedPoly.field, undeterminedPoly.poly, Array(range))
  }

  def main(args: Array[String]): Unit = {
    val x = RealPolyWithRange("x", IntRange(1))
    println(x.range)
    println(x.expr)

    val y = RealPolyWithRange("y", IntRange(2, -1))
    val z = x - x
    println(z.range)
    println(z.expr)

    val unknownX = RealPolyWithRange(IntRange(0, 1))
    val unknownZ = unknownX - unknownX
    println(unknownZ.range)
    println(unknownZ.expr)
  }
}
