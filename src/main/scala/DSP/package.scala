import breeze.linalg._
import breeze.numerics._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

package object DSP {

  // debug mode
  var debug = false

  def printlnWhenDebug(x: Any) = if (debug) println(x)

  // addition for SFix that keeps the carry
  implicit class mySFix(val sfix: SFix) {
    def +^(that: SFix) = {
      val lsbDif = (sfix.maxExp - sfix.bitCount) - (that.maxExp - that.bitCount)
      val left = if (lsbDif > 0) sfix.raw << lsbDif else sfix.raw
      val right = if (lsbDif < 0) that.raw << -lsbDif else that.raw
      val ret = SFix((Math.max(sfix.maxExp, that.maxExp) + 1) exp, (Math.max(left.getBitsWidth, right.getBitsWidth) + 1) bits)
      ret.raw := left +^ right
      ret
    }

    def -^(that: SFix) = {
      val lsbDif = (sfix.maxExp - sfix.bitCount) - (that.maxExp - that.bitCount)
      val left = if (lsbDif > 0) sfix.raw << lsbDif else sfix.raw
      val right = if (lsbDif < 0) that.raw << -lsbDif else that.raw
      val ret = SFix((Math.max(sfix.maxExp, that.maxExp) + 1) exp, (Math.max(left.getBitsWidth, right.getBitsWidth) + 1) bits)
      ret.raw := left -^ right
      ret
    }

    def unary_- = {
      val ret = SFix(sfix.maxExp exp, sfix.minExp exp)
      ret.raw := -sfix.raw
      ret
    }

  }

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

//  def phaseType(resolution: Double = 0.001) = MySFix(Pi, -Pi, resolution) // [-Pi, Pi] for phase
  def unitType(resolution: Double = 0.001) = MySFix(1, -1, resolution)

  def globalType = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  def shortGlobalType = SFix((naturalWidth / 2) exp, -(fractionalWidth / 2) exp)

  val rand = new Random()

  // generate random value in a range defined by SFix format
  def randData(bits: Int = naturalWidth) = {
    ((rand.nextDouble() - 0.5) * 2 * pow(2, bits))
  }

  val testFFTLength = 8

  def sameFixed(a: Double, b: Double) = (abs(a - b) / abs((a + b) / 2)) < 0.1 || scala.math.abs(a - b) < 1.0

  def sameFixedSeq(v1: IndexedSeq[Double], v2: IndexedSeq[Double]) =
    v1.zip(v2).forall { case (c1, c2) => sameFixed(c1, c2) }

  def sameFixedVector(v1: DenseVector[Double], v2: DenseVector[Double]) = sameFixedSeq(v1.toArray, v2.toArray)

  //  def Double2Fix(value: Double) = floor(value * (1 << 4)).toInt // convert Double to valid stimulus for simulation
  def Double2Fix(value: Double, fw: Int = fractionalWidth) = floor(value * (1 << fw)).toInt // convert Double to valid stimulus for simulation
  //  def Fix2Double(value: SFix) = value.raw.toBigInt.toDouble / pow(2, 4)
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

  // ALGO 2.2
  def optimalCSD(num: Int): String = {
    val pattern0 = "11+0".r
    val pattern1 = "1101".r
    val pattern2 = "901".r

    var string = num.toBinaryString.reverse + "0"
    var done0 = false
    var done1 = false
    var done2 = false

    while (!done0) {
      val sub = pattern0.findFirstIn(string).getOrElse {
        done0 = true
        " "
      }
      string = string.replace(sub, "9" + "0" * (sub.length - 2) + "1")
    }

    while (!done1) {
      val sub = pattern1.findFirstIn(string).getOrElse {
        done1 = true
        " "
      }
      string = string.replaceFirst(sub, "9011")

    }

    while (!done2) {
      val sub = pattern2.findFirstIn(string).getOrElse {
        done2 = true
        " "
      }
      string = string.replaceFirst(sub, "110")
    }

    string.reverse.dropWhile(_ == '0')
  }

  def verifyCSD(res: String, num: Int): Boolean = {
    (0 until res.length).map { i =>
      res(i) match {
        case '0' => 0
        case '1' => 1 * pow(2, i)
        case '9' => -1 * pow(2, i)
      }
    }.sum == num
  }
}
