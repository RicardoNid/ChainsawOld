import breeze.linalg._
import breeze.numerics._
import breeze.numerics.constants.Pi
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

package object Chainsaw {

  trait BaseTypeFactory extends BoolFactory with BitsFactory with UIntFactory with SIntFactory with VecFactory with SFixFactory with UFixFactory with TypeFactory

  // debug mode
  var debug = false
  def printlnWhenDebug(x: Any) = if (debug) println(x)

  def MySFix(maxValue: Double, minValue: Double, resolution: Double): SFix = {
    require(maxValue >= 0)
    val maxExp0 = log2Up(floor(maxValue + 1).toInt)
    val maxExp1 = log2Up(abs(minValue).toInt)
    val maxExp = Array(maxExp0, maxExp1).max
    val minExp = -log2Up(abs(1 / resolution).toInt)
    SFix(maxExp exp, minExp exp)
  }

  def MySFix(maxValue: Double, resolution: Double): SFix = MySFix(maxValue, -maxValue, resolution)

  /** SFix literal with an appropriated bitWidth
   *
   * @param value
   * @param resolution
   * @return
   */
  def MySF(value: Double, resolution: Double = 1.0) = {
    val tmp = MySFix(value, resolution)
    tmp := value
    tmp
  }

  // typedefs and numeric considerations
  val naturalWidth = 6
  val fractionalWidth = 10
  val bitWidth = naturalWidth + fractionalWidth

  def phaseType(resolution: Double = 0.001) = MySFix(Pi, -Pi, resolution) // [-Pi, Pi] for phase
  def unitType(resolution: Double = 0.001) = MySFix(1, -1, resolution)

  def globalType = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  def shortGlobalType = SFix((naturalWidth / 2) exp, -(fractionalWidth / 2) exp)

  val testFFTLength = 8

  def sameFixed(a: Double, b: Double) = (abs(a - b) / abs((a + b) / 2)) < 0.05 || scala.math.abs(a - b) < 0.1

  def sameFixedSeq(v1: IndexedSeq[Double], v2: IndexedSeq[Double]) =
    v1.zip(v2).forall { case (c1, c2) => sameFixed(c1, c2) }

  def sameFixedVector(v1: DenseVector[Double], v2: DenseVector[Double]) = sameFixedSeq(v1.toArray, v2.toArray)

  //  def Double2Fix(value: Double) = floor(value * (1 << 4)).toInt // convert Double to valid stimulus for simulation
  //  def Fix2Double(value: SFix) = value.raw.toBigInt.toDouble / pow(2, 4)
  def Double2Fix(value: Double, fw: Int = fractionalWidth) = floor(value * (1 << fw)).toInt // convert Double to valid stimulus for simulation
  def Fix2Double(value: SFix, fw: Int = fractionalWidth) = value.raw.toBigInt.toDouble / pow(2, fw)

  // OPTIMIZE: implement prime & factor by table
  def isPrime(n: Int): Boolean = {
    if (n <= 1)
      false
    else if (n == 2)
      true
    else
      !(2 until n).exists(n % _ == 0)
  }

  def factorize(N: Int): ArrayBuffer[Int] = {
    if (isPrime(N)) ArrayBuffer(N)
    else {
      val factor = (2 until N).find(N % _ == 0).get
      val result = factorize(N / factor)
      result.insert(0, factor)
      result
    }
  }

  // ALGO 2.1
  def classicCSD(num: Int): String = {
    val pattern = "11+0".r

    var string = num.toBinaryString.reverse + "0"
    var done = false
    while (!done) {
      val sub: Option[String] = pattern.findFirstIn(string)
      sub match {
        case Some(x) => string = string.replaceFirst(x, "9" + "0" * (x.length - 2) + "1")
        case None => done = true
      }
    }
    string.reverse
  }

  val DSPRand = new Random(42) // using this as global random gen, with a fixed seed

  //  type Field = MultivariateRing[IntZ]
  //  type Poly = MultivariatePolynomial[BigInteger]
  //  type Term = Monomial[BigInteger]

  def bs2i(bs: String) = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum
  def bs2i2c(bs: String) = {
    val values = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }
    values.dropRight(1).sum - values.last
  }
}
