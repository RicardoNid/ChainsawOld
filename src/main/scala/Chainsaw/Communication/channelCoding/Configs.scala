package Chainsaw.Communication.channelCoding

import Chainsaw._
import spinal.core._

/** Parameters to specify a convolutional encoding scheme
 *
 * learn more [[]]
 *
 * @param codeGens a n*k 2-dimensional array of generator polynomial
 * @param radix    the radix of generator polynomial
 */
case class ConvConfig(codeGens: Array[Array[Int]], radix: Int = 10) {
  val n: Int = codeGens.length // number of input bits
  val k: Int = codeGens.head.length // number of output bits
  val decimalCodeGens: Array[Array[Int]] = codeGens.map(_.map(value => BigInt(value.toString, radix).toInt))
  val ms: Array[Int] = decimalCodeGens.map(_.map(log2Up(_)).max - 1) // numbers of registers for each channel
  val binaryCodeGens: Array[Array[String]] = decimalCodeGens.zip(ms).map { case (polys, m) => polys.map(_.toBinaryString.padToLeft(m + 1, '0')) }

  //  println(decimalCodeGens.map(_.mkString(" ")).mkString("\n"))
  //  println(binaryCodeGens.map(_.mkString(" ")).mkString("\n"))
}

object ConvConfig {
  def apply(codeGens: Array[Int], radix: Int): ConvConfig = new ConvConfig(Array(codeGens), radix)
}