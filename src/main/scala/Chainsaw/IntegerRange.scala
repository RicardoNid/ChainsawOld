package Chainsaw

import spinal.core._

import scala.math.{ceil, floor}

case class IntegerRange(low: Int, high: Int) {

  require(low <= high)

  def *(multiple: Int) = IntegerRange(multiple * low, multiple * high)

  def *(that: IntegerRange) = {
    val cands = Seq(
      this.low * that.low,
      this.high * that.low,
      this.low * that.high,
      this.high * that.high
    )
    IntegerRange(cands.min, cands.max) // TODO: is this correct?
  }

  def %(modulo: Int) = {
    require(isPow2(modulo))
    IntegerRange(0, modulo - 1)
  }

  def /(modulo: Int) = {
    require(isPow2(modulo))
    IntegerRange(floor(low / modulo.toDouble).toInt, floor(high / modulo.toDouble).toInt)
  }

  def +(that: IntegerRange) = IntegerRange(low + that.low, high + that.high)

  def -(that: IntegerRange) = IntegerRange(low - that.high, high - that.low)

}

object IntegerRange {
  def main(args: Array[String]): Unit = {

    def kred(c: IntegerRange) = {

      val cl = c % 256
      val ch = c / 256
      val cPrime = cl * 13 - ch

      val cPrimel = cPrime % 256
      val cPrimeh = cPrime / 256
      cPrimel * 13 - cPrimeh
    }

    val base = IntegerRange(0, 3328)

    //    def kred(c: IntegerRange) = {
    //      printlnRed(s"into kred $c")
    //      base // while doing correction in kred
    //    }

    def correction(base: IntegerRange, possible: IntegerRange, modulo: Int) = {
      val lowGap = base.low - possible.low
      val addCorrection = if (lowGap > 0) ceil(lowGap / modulo.toDouble).toInt else 0
      val highGap = possible.high - base.high
      val miusCorrection = if (highGap > 0) ceil(highGap / modulo.toDouble).toInt else 0
      addCorrection + miusCorrection
    }

    def allCorrections(base: IntegerRange) = {

      def cfCorrection(base: IntegerRange) = {
        val uPvw = correction(base, base + kred(base * base), 3329) // u + vw
        println(s"u + vw, ${base + kred(base * base)}")
        val uMvw = correction(base, base - kred(base * base), 3329) // u - vw
        println(s"u - vw, ${base - kred(base * base)}")
        println(uPvw, uMvw)
        uPvw + uMvw
      }

      def gsCorrection(base: IntegerRange) = {
        val uPv = correction(base, base + base, 3329) // u + v
        println(s"u + v, ${base + base}")
        val uMv_w = correction(base, kred((base - base) * base), 3329) // u - vw
        println(s"(u - v)w, ${kred((base - base) * base)}")
        println(uPv, uMv_w)
        uPv + uMv_w
      }

      2 * gsCorrection(base) + cfCorrection(base)
    }

    println(kred(base * base))
    println(correction(base, kred(base), 3329))
    println(allCorrections(base))

    // TODO: give a function defining "self-validated with corrections"

  }
}
