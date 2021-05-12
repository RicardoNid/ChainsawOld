package Chainsaw

//import scala.collection.mutable.Map

import scala.math._

class AffineForm(val constant: Double, val rangeTerms: Map[String, Double], val roundingTerms: Map[String, Double]) {

  def rad = rangeTerms.values.map(_.abs).sum

  def getRange(key: String) = rangeTerms.getOrElse(key, 0.0)

  def getRounding(key: String) = roundingTerms.getOrElse(key, 0.0)

  def unary_-() = new AffineForm(-constant,
    rangeTerms.map { case (str, d) => (str, -d) },
    roundingTerms.map { case (str, d) => (str, -d) })

  def doAddSub(that: AffineForm, add: Boolean) = {
    val constant = if (add) this.constant + that.constant else this.constant - that.constant
    val rangeTerms =
      this.rangeTerms.keySet.union(that.rangeTerms.keySet)
        .map { key =>
          key -> (if (add) this.getRange(key) + that.getRange(key) else this.getRange(key) - that.getRange(key))
        }
        .toMap
    val roundingTerms = this.roundingTerms ++ that.roundingTerms
    new AffineForm(constant, rangeTerms, roundingTerms)
  }
  def +(that: AffineForm) = doAddSub(that, true)
  def -(that: AffineForm) = doAddSub(that, false)

  def *(thatConstnt: Double) = new AffineForm(constant * thatConstnt,
    rangeTerms.map { case (str, d) => (str, d * thatConstnt) },
    roundingTerms.map { case (str, d) => (str, d * thatConstnt) })

  def doAddSub(thatConstant: Double, add: Boolean) = new AffineForm(
    if (add) constant + thatConstant else constant - thatConstant,
    rangeTerms.map(term => term), // copy
    roundingTerms.map(term => term)
  )
  def +(thatConstant: Double) = doAddSub(thatConstant, true)
  def -(thatConstant: Double) = doAddSub(thatConstant, false)

  override def toString: String = s"$constant + " +
    s"${rangeTerms.map { case (str, d) => s"$d$str" }.mkString(" + ")} + " +
    s"${roundingTerms.map { case (str, d) => s"$d$str" }.mkString(" + ")}"

}

object AffineForm {

  /** Affine operation f(x,y) = ax + by + c
   *
   * @param resolution
   */
  def affine(x: AffineForm, y: AffineForm, a: Double, b: Double, c: Double) = {
    val constant = a * x.constant + b * y.constant + c
    val rangeTerms =
      x.rangeTerms.keySet.union(y.rangeTerms.keySet)
        .map { key => key -> (a * x.rangeTerms.getOrElse(key, 0.0) + b * y.rangeTerms.getOrElse(key, 0.0)) }
        .toMap
    val roundingTerms = x.roundingTerms ++ y.roundingTerms
    new AffineForm(constant, rangeTerms, roundingTerms)
  }

  def main(args: Array[String]): Unit = {

    val affineForm0 = new AffineForm(0, Map("x" -> 1.0, "y" -> 2.0), Map("r0" -> 1.0))
    val affineForm1 = new AffineForm(2, Map("y" -> 1.0, "z" -> 2.0), Map("r1" -> 1.0))

    println(affineForm0)
    println(affineForm0.rad)
    println(-affineForm0)
    println(affineForm0 + affineForm1)
    println(affineForm0 + 3)
    println(affineForm0 * 3)
    println(affine(affineForm0, affineForm1, 2, 3, 1))

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
