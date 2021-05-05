package DSP

import DSP.OptionalPoly.undeterminedPoly
import cc.redberry.rings.poly.multivar.MonomialOrder._
import cc.redberry.rings.scaladsl.{IntZ, MultivariateRing, Z, _}

class OptionalPoly(val field: Field, val poly: Poly) {

  val expr = poly.toString(field.variables: _*)

  val undetermined = field.variables.contains("none")

  def toUnionField(that: OptionalPoly) = {
    val unionField = new MultivariateRing(Z, this.field.variables.union(that.field.variables).distinct.sorted, GREVLEX)
    (unionField, unionField.parse(this.expr), unionField.parse(that.expr))
  }

  def binaryOperation(that: OptionalPoly, operator: (Field, Poly, Poly) => Poly) = {
    if (this.undetermined || that.undetermined) undeterminedPoly
    else {
      val (unionfield, newThis, newThat) = toUnionField(that)
      new OptionalPoly(unionfield, operator(unionfield, newThis, newThat))
    }
  }

  def binaryOperationOnConstant(constant: Int, operator: (Field, Poly, IntZ) => Poly) = {
    if (this.undetermined) undeterminedPoly
    else new OptionalPoly(field, operator(field, poly, Z(constant)))
  }

  def +(that: OptionalPoly) = binaryOperation(that, _.add(_, _))

  def -(that: OptionalPoly) = binaryOperation(that, _.subtract(_, _))

  def *(that: OptionalPoly) = binaryOperation(that, _.multiply(_, _))

  def +(constant: Int) = binaryOperationOnConstant(constant, _.addConstant(_, _))

  def -(constant: Int) = binaryOperationOnConstant(constant, _.subtractConstant(_, _))

  def *(constant: Int) = binaryOperationOnConstant(constant, _.multiplyConstant(_, _))

  def <<(shift: Int) = binaryOperationOnConstant(1 << shift, _.multiplyConstant(_, _))

  def >>(shift: Int) = undeterminedPoly
}

object OptionalPoly {

  /** The extrance of a signal, which is always represented by a independent variable
   */
  def apply(name: String): OptionalPoly = { // the name should not contain any arithmetic operator
    val field = new MultivariateRing(Z, Array(name), GREVLEX) // ordinary integer polynomial that we use
    val poly = field.parse(name)
    new OptionalPoly(field, poly)
  }

  def undeterminedPoly = OptionalPoly("none")

  def main(args: Array[String]): Unit = {
    val a = OptionalPoly("a")
    val b = OptionalPoly("b")
    val c = OptionalPoly("c")

    def test(poly: OptionalPoly, golden: String) = assert(poly.expr == golden, s"yours: ${poly.expr}")

    test(a + b + c, "c+b+a")
    test(a + (b << 2) + c, "c+4*b+a")
    test((a + b) * (b + c), "b*c+a*c+b^2+a*b")
    test(a + (b >> 2) + c, "none")
    test(a + b * c * 3 + c, "c+a+3*b*c")
    test(a - b * c * 3 + c, "c+a-3*b*c")
    test(a - b * c * 3 + c + 5, "5+c+a-3*b*c")

    def formula(poly0: OptionalPoly, poly1: OptionalPoly) = assert(poly0.poly == poly1.poly, s"${poly0.poly} ${poly1.poly}")

    formula(a + b, b + a)
    formula(a + b + c, a + (b + c))
    formula(a * b, b * a)
    formula(a * b * c, a * (b * c))
    formula((a + b) * c, a * c + b * c)
    formula(c * (a + b), a * c + b * c)
  }
}


