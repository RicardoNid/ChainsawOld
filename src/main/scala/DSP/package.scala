import breeze.numerics._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

package object DSP {

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
  }

  // typedefs
  val naturalWidth = 6
  val fractionalWidth = 10
  val bitWidth = naturalWidth + fractionalWidth

  def data = SFix(peak = naturalWidth exp, resolution = -fractionalWidth exp)

  def shortType = SFix(4 exp, -4 exp)

  val rand = new Random()

  // generate random value in a range defined by SFix format
  def randData(bits: Int = naturalWidth) = {
    ((rand.nextDouble() - 0.5) * 2 * pow(2, bits))
  }

  val testFFTLength = 8

  def sameFixed(a: Double, b: Double) = abs(a - b) / abs((a + b) / 2) < 0.01 || scala.math.abs(a - b) < 0.1

  //  def Double2Fix(value: Double) = floor(value * (1 << 4)).toInt // convert Double to valid stimulus for simulation
  def Double2Fix(value: Double) = floor(value * (1 << fractionalWidth)).toInt // convert Double to valid stimulus for simulation
  //  def Fix2Double(value: SFix) = value.raw.toBigInt.toDouble / pow(2, 4)
  def Fix2Double(value: SFix) = value.raw.toBigInt.toDouble / pow(2, fractionalWidth)

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
