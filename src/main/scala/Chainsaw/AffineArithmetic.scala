package Chainsaw

//import scala.collection.mutable.Map

import scala.math._

class AffineForm(val constant: Double, val rangeTerms: Map[String, Double]) {

  def rad = rangeTerms.values.map(_.abs).sum

  def upper = constant + rad

  def lower = constant - rad

  def getRange(key: String) = rangeTerms.getOrElse(key, 0.0)

  def unary_-() = new AffineForm(-constant,
    rangeTerms.map { case (str, d) => (str, -d) })

  // naturally affine operations
  def doAddSub(that: AffineForm, add: Boolean) = {
    val constant = if (add) this.constant + that.constant else this.constant - that.constant
    val rangeTerms =
      this.rangeTerms.keySet.union(that.rangeTerms.keySet)
        .map { key =>
          key -> (if (add) this.getRange(key) + that.getRange(key) else this.getRange(key) - that.getRange(key))
        }
        .toMap
    new AffineForm(constant, rangeTerms)
  }
  def +(that: AffineForm) = doAddSub(that, true)
  def -(that: AffineForm) = doAddSub(that, false)

  def *(thatConstnt: Double) = new AffineForm(constant * thatConstnt,
    rangeTerms.map { case (str, d) => (str, d * thatConstnt) })

  def doAddSub(thatConstant: Double, add: Boolean) = new AffineForm(
    if (add) constant + thatConstant else constant - thatConstant,
    rangeTerms.map(term => term) // copy
  )
  def +(thatConstant: Double) = doAddSub(thatConstant, true)
  def -(thatConstant: Double) = doAddSub(thatConstant, false)

  // non-affine operations
  def *(that: AffineForm) = {
    val a = that.constant
    val b = this.constant
    val c = -(this.constant * that.constant)
    val delta = this.rad * that.rad
    AffineForm.affine(this, that, a, b, c, delta)
  }

  override def toString: String = s"$constant + " +
    s"${rangeTerms.map { case (str, d) => s"$d$str" }.mkString(" + ")}"
}

object AffineForm {

  /** Affine operation f(x,y) = ax + by + c
   *
   * @param resolution
   */
  def affine(x: AffineForm, y: AffineForm, a: Double, b: Double, c: Double, delta: Double) = {
    val constant = a * x.constant + b * y.constant + c
    val rangeTerms =
      x.rangeTerms.keySet.union(y.rangeTerms.keySet)
        .map { key => key -> (a * x.rangeTerms.getOrElse(key, 0.0) + b * y.rangeTerms.getOrElse(key, 0.0)) }
        .toMap + (newSym() -> delta)
    new AffineForm(constant, rangeTerms)
  }

  var symIndex = -1

  /** Global symbol name supplier
   */
  def newSym() = {
    symIndex += 1
    s"sigma$symIndex"
  }

  def main(args: Array[String]): Unit = {

    val affineForm0 = new AffineForm(0, Map("x" -> 1.0, "y" -> 2.0))
    val affineForm1 = new AffineForm(2, Map("y" -> 1.0, "z" -> 2.0))

    println(affineForm0)
    println(affineForm0.rad)
    println(-affineForm0)
    println(affineForm0 + affineForm1)
    println(affineForm0 + 3)
    println(affineForm0 * 3)
    println(affine(affineForm0, affineForm1, 2, 3, 1, 1))

    val af0 = new AffineForm(10, Map("x" -> 2.0, "r" -> 1.0))
    val af1 = new AffineForm(10, Map("x" -> -2.0, "s" -> 1.0))

    println((af0 * af1).lower)
    println((af0 * af1).upper)

    implicit val resolution = 0
  }
}

class AffineArithmetic {

}

object AffineArithmetic {

  implicit class numericOp(value: Double) {

    /** Rounding up to the nearest representable value
     *
     * @param resolution resolution of current format
     */
    def roundUp(implicit resolution: Int) = {
      val ulp = pow(2.0, resolution)
      ceil(value / ulp) * ulp
    }

    def roundDown(implicit resolution: Int) = {
      val ulp = pow(2.0, resolution)
      floor(value / ulp) * ulp
    }
  }
}
