package FTN

import spinal.core._
import spinal.lib._

import scala.math.pow //  for digital signal processing

/** Convolutional code encoder
 *
 * @param convConstLength constraint lenght of convolutional code
 * @param convCodeGen     generator polynomial provided in octal numbers
 */
class Convenc(convConstLength: Int, convCodeGen: Array[Int]) extends Component {

  def oct2dec(oct: Int) =
    oct.toString.toCharArray.map(_.asDigit).reverse // get digits
      .zipWithIndex.map { case (digit, i) => digit * pow(8, i) }.sum.toInt // weighted sum
  def padding(string: String, length: Int) = "0" * (length - string.length) + string

  val binaryPoly = convCodeGen.map(number => padding(oct2dec(number).toBinaryString, convConstLength))

  val input = in Bool()
  val output = out(Vec(Bool(), convCodeGen.length))

  val srl = History(input, convConstLength, True)

  output := Vec(binaryPoly.map(poly => poly.zip(srl).filter(_._1 == '1').map(_._2).asBits().xorR))
}

object Convenc {

  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new Convenc(ConvConstLength, ConvCodeGen))
  }
}
