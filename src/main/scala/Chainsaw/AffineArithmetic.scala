/*
Reference book = Self-Validated Numerical Methods and Applications

The bold parts in ScalaDoc are directly quoted from the book, which are terminologies or routines.
 */
package Chainsaw

import java.util.function.Supplier

/** Static methods of affine arithmetic
  *
  * @see
  *   ''Self-Validated Numerical Methods and Applications''
  *   [[https://nonchalant-frill-fc5.notion.site/Self-Validated-Numerical-Methods-and-Applications-03e87c21f41c47db9ee1d0e86e22e998]]
  */
object AffineArithmetic {

  /** Affine operation f(x,y) = ax + by + c
    *
    * @see
    *   routine '''AA.affine'''
    */
  def affine(x: AffineForm, y: AffineForm, a: Double, b: Double, c: Double, delta: Double): AffineForm = {
    val constant = a * x.constant + b * y.constant + c
    val intervalTerms =
      x.intervalTerms.keySet
        .union(y.intervalTerms.keySet)
        .map { key => key -> (a * x.intervalTerms.getOrElse(key, 0.0) + b * y.intervalTerms.getOrElse(key, 0.0)) }
        .toMap
        .filterNot(_._2 == 0.0) + (newSym() -> delta)
    new AffineForm(constant, intervalTerms)
  }

  /** Global symbol name supplier, implemented as java supplier
    */
  val symbolSupplier: Supplier[String] = new Supplier[String] {
    var symbolIndex = -1

    override def get(): String = {
      symbolIndex += 1
      s"sigma$symbolIndex"
    }
  }

  /** @see
    *   routine '''newSym'''
    */
  def newSym() = symbolSupplier.get()
}

/** Representation and methods of '''affine form'''
  *
  * @param constant
  *   '''central value'''
  * @param intervalTerms
  *   each term is a '''noise symbol''' with its coefficient
  * @see
  *   [[https://www.notion.so/Self-Validated-Numerical-Methods-and-Applications-30f49c20328848798c0309a32d870d65 Self-Validated Numerical Methods and Applications]]
  */
class AffineForm(val constant: Double, val intervalTerms: Map[String, Double]) {

  override def clone = new AffineForm(constant, intervalTerms.map(term => term))

  /** Radius of the interval
    *
    * @see
    *   '''AA.rad'''
    */
  def rad        = intervalTerms.values.map(_.abs).sum
  def isConstant = rad == 0.0

  /** Upper and lower bound of the interval
    *
    * @see
    *   routine '''IA.from.AA'''
    */
  def upper = constant + rad
  def lower = constant - rad

  /** Get the coefficient of a interval symbol by its name
    */
  def getIntervalTerm(key: String) = intervalTerms.getOrElse(key, 0.0)

  /** Operations below are naturally affine operations, including
    *
    * '''AA.neg'''
    *
    * addition/subtraction between affine forms
    *
    * multiplication with constants
    *
    * addition/subtraction with constants
    */
  def unary_-() = new AffineForm(-constant, intervalTerms.map { case (str, d) => (str, -d) })

  // naturally affine operations
  def doAddSub(that: AffineForm, add: Boolean): AffineForm = {
    println(s"addSub here: $this, $that")
    val constant = if (add) this.constant + that.constant else this.constant - that.constant
    val intervalTerms =
      this.intervalTerms.keySet
        .union(that.intervalTerms.keySet)
        .map { key =>
          key -> (if (add) this.getIntervalTerm(key) + that.getIntervalTerm(key) else this.getIntervalTerm(key) - that.getIntervalTerm(key))
        }
        .toMap
        .filterNot(_._2 == 0.0)
    println(intervalTerms.keySet.mkString(" "))
    new AffineForm(constant, intervalTerms)
  }
  def +(that: AffineForm): AffineForm = doAddSub(that, add = true)
  def -(that: AffineForm): AffineForm = doAddSub(that, add = false)

  def doAddSub(thatConstant: Double, add: Boolean) = new AffineForm(
    if (add) constant + thatConstant else constant - thatConstant,
    intervalTerms.map(term => term) // copy
  )
  def +(thatConstant: Double): AffineForm = doAddSub(thatConstant, add = true)
  def -(thatConstant: Double): AffineForm = doAddSub(thatConstant, add = false)

  def *(thatConstant: Double) = new AffineForm(constant * thatConstant, intervalTerms.map { case (str, d) => (str, d * thatConstant) })

  /** Operations below are non-affine operations, implemented by affine approximation
    *
    * '''AA.mul'''
    */
  def *(that: AffineForm): AffineForm = {
    val a     = that.constant
    val b     = this.constant
    val c     = -(this.constant * that.constant)
    val delta = this.rad * that.rad
    AffineArithmetic.affine(this, that, a, b, c, delta)
  }

  override def toString: String = s"AffineForm $constant + " +
    s"${intervalTerms.map { case (str, d) => s"$d$str" }.mkString(" + ")}"
}

object AffineForm {

  /** Native factory
    */
  def apply(constant: Double, intervalTerms: Map[String, Double]): AffineForm =
    new AffineForm(constant, intervalTerms)

  def apply(constant: Double): AffineForm =
    AffineForm(constant, Map[String, Double]())

  /** The most commonly used factory which initialize an affine form from a interval [lower, upper]
    */
  def apply(lower: Double, upper: Double): AffineForm = {
    require(upper >= lower, "upper should be greater or equal to lower")
    val constant = (upper + lower) / 2
    val rad      = (upper - lower) / 2
    AffineForm(constant, Map(AffineArithmetic.newSym() -> rad))
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
    println(AffineArithmetic.affine(affineForm0, affineForm1, 2, 3, 1, 1))

    val af0 = new AffineForm(10, Map("x" -> 2.0, "r" -> 1.0))
    val af1 = new AffineForm(10, Map("x" -> -2.0, "s" -> 1.0))
    println((af0 * af1).lower)
    println((af0 * af1).upper)

    AffineArithmetic.symbolSupplier

    val af2 = AffineForm(-1, 1)
    val af3 = AffineForm(-1, 1)
    println(af2)
    println(af3)
    println(af2 * af3)
  }
}
