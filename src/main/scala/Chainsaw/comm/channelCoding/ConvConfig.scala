package Chainsaw.comm.channelCoding

import Chainsaw._
import spinal.core._

/** Parameters to specify a convolutional encoding scheme
 *
 * @param codeGens a n*k 2-dimensional array of generator polynomial
 * @param radix    the radix of generator polynomial
 * @see we adopt symbols of ''Coding Theory'' Chap3.1
 */
case class ConvConfig(codeGens: Array[Array[Int]], radix: Int = 8) {
  val n: Int = codeGens.length // number of input bits
  val k: Int = codeGens.head.length // number of output bits
  val decimalCodeGens: Array[Array[BigInt]] = codeGens.map(_.map(value => BigInt(value.toString, radix)))
  val ms: Array[Int] = decimalCodeGens.map(_.map(log2Up(_)).max - 1) // numbers of registers for each channel("m" stands for memory)
  val binaryCodeGens: Array[Array[String]] = decimalCodeGens.zip(ms).map { case (polys, m) => polys.map(_.toString(2).padToLeft(m + 1, '0')) }
  val codeRate = n / k.toDouble
}

object ConvConfig {
  def apply(codeGens: Array[Int], radix: Int): ConvConfig = new ConvConfig(Array(codeGens), radix)
}