package Chainsaw.algos

import Chainsaw._
import breeze.linalg._
import breeze.numerics._
import spinal.core.{isPow2, log2Up}

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

object Qam {

  def getSymbols(modulationOrder: Int) = {
    require(isPow2(modulationOrder))
    require(modulationOrder <= 256, "modulation order higher than 256 has not been supported yet")
    require(modulationOrder != 32 && modulationOrder != 128, "modulation order 32 & 128 has not been supported yet")

    val bits = log2Up(modulationOrder)

    def genSymbol(order: Int, bits: Int) = {
      require(order < (1 << bits))
      val lowBits   = bits / 2 // when bits is odd, lowBits is smaller than highBits
      val highBits  = bits - lowBits
      val highOrder = Gray.fromGray(order >> lowBits, highBits)
      val lowOrder  = Gray.fromGray(order % (1 << lowBits), lowBits)
      val realValue = (highOrder - (1 << (highBits - 1))) * 2.0 + 1
      val imagValue = -((lowOrder - (1 << (lowBits - 1))) * 2.0 + 1)
      Seq(realValue, imagValue)
    }

    def genSymbols(modulationOrder: Int): Seq[Double] = (0 until modulationOrder).flatMap(genSymbol(_, bits))

    val rawData = modulationOrder match {
      case 2 => Seq(-1.0, 0.0, 1.0, 0.0)
      case modulationOrder => genSymbols(modulationOrder)
    }

    rawData.grouped(2).map(pair => BComplex(pair(0), pair(1))).toSeq.asDv
  }

  def getAveragePower(modulationOrder: Int) = {
    val realVector = getSymbols(modulationOrder).map(_.real)
    val imagVector = getSymbols(modulationOrder).map(_.imag)
    sqrt(sum(pow(DenseVector.vertcat(realVector, imagVector), 2)) / modulationOrder) // rms of complex
  }

  // TODO: the relationship between gray & bin is 2-D in Qam
  // more specifically, it adopted gray code on two axis independently, for example

  /** use gray code order by default as there's no extra effort implementing it
    */
  def qammod(data: DenseVector[Int], modulationOrder: Int): DenseVector[BComplex] = {
    val averagePower = getAveragePower(modulationOrder)
    val lut          = getSymbols(modulationOrder)
    data.map(lut(_) / averagePower)
  }

  def qamdemod(data: DenseVector[BComplex], modulationOrder: Int): DenseVector[Int] = {
    require(isPow2(modulationOrder))
    require(modulationOrder <= 256, "demodulation order higher than 256 has not been supported yet")
    require(modulationOrder != 32 && modulationOrder != 128, "demodulation order 32 & 128 has not been supported yet")

    val bits     = log2Up(modulationOrder)
    val lowBits  = bits / 2 // when bits is odd, lowBits is smaller than highBits
    val highBits = bits - lowBits

    def getThresholds(bits: Int) = (0 +: (bits - 1 to 1 by -1).map(1 << _)).map(_ / getAveragePower(modulationOrder))

    def folding(value: Double, bits: Int): Seq[Boolean] = {
      val thresholds  = getThresholds(bits)
      val valueBuffer = ArrayBuffer[Double](value)
      val bitBuffer   = ArrayBuffer[Boolean]()
      thresholds.foreach { threshold =>
        val diff = valueBuffer.last - threshold
        bitBuffer += diff < 0
        valueBuffer += diff.abs
      }
      bitBuffer
    }

    def bools2Int(bools: Seq[Boolean]) = bools.reverse.zipWithIndex.map { case (bool, i) => if (bool) 1 << i else 0 }.sum

    data.map { complex =>
      val bools     = folding(complex.real, highBits) ++ folding(complex.imag, lowBits)
      val trueBools = !bools.head +: bools.tail
      bools2Int(trueBools)
    }
  }

  def main(args: Array[String]): Unit = {

    def printRaw(modulationOrder: Int) = {
      val raw = MatlabRefs
        .qammod(DenseVector.tabulate(modulationOrder)(i => i), modulationOrder)
        .toArray
        .grouped(16)
        .map(_.map(complex => s"${complex.real}, ${complex.imag}").mkString(", "))
        .mkString(",\n")
      println(s"case $modulationOrder => Seq($raw)")
    }

    Seq(4, 8, 16, 32, 64, 128, 256).foreach(printRaw(_))
  }
}
