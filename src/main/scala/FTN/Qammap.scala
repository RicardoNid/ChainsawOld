package FTN

import Chainsaw.{eng, _}
import FTN.Qammap.qammap
import com.mathworks.matlab.types.Complex
import spinal.core.{BitCount, _}

class Qammap(input: UInt, bitsAllocated: Int, resolution: ExpNumber = 0 exp) extends ImplicitArea[ComplexReal] {
  val ret = ComplexReal(
    Real(0, (1 << bitsAllocated) - 1, resolution),
    Real(0, (1 << bitsAllocated) - 1, resolution))
  switch(input) {
    (0 until (1 << bitsAllocated)).foreach(i =>
      is(U(i, bitsAllocated bits)) {
        ret.real := qammap(i, bitsAllocated bits).real
        ret.imag := qammap(i, bitsAllocated bits).imag
      }
    )
  }
  override def implicitValue: ComplexReal = ret
}

object Qammap {

  def apply(input: UInt, bitsAllocated: Int, resolution: ExpNumber): Qammap = new Qammap(input, bitsAllocated, resolution)
  
  /** Invoke matlab to generate result of QAM map from 2 bits to 8 bits
   *
   */
  def genHardCodedSource = {
    (2 to 8).foreach { i =>
      val numberAsBits = (0 until 1 << i)
        .map(_.toBinaryString.reverse.padTo(i, '0').reverse)
        .flatten
        .map(_.asDigit)
        .map(Array(_)).toArray
      eng.putVariable("bits", numberAsBits)
      eng.eval(s"symbols = qammod(bits, 2^$i, 'gray', 'InputType', 'bit');")
      val symbols = eng.getVariable("symbols").asInstanceOf[Array[Complex]]
      println(symbols.mkString(" "))
    }
    eng.close()
  }

  // TODO: implement gray coding and QAM coding, replace the hard-coded part
  val hardCodingSource =
    "-1.0 + 0.0i 1.0 + 0.0i\n" +
      "-1.0 + 1.0i -1.0 + -1.0i 1.0 + 1.0i 1.0 + -1.0i\n" +
      "-3.0 + 1.0i -3.0 + -1.0i -1.0 + 1.0i -1.0 + -1.0i 3.0 + 1.0i 3.0 + -1.0i 1.0 + 1.0i 1.0 + -1.0i\n" +
      "-3.0 + 3.0i -3.0 + 1.0i -3.0 + -3.0i -3.0 + -1.0i -1.0 + 3.0i -1.0 + 1.0i -1.0 + -3.0i -1.0 + -1.0i 3.0 + 3.0i 3.0 + 1.0i 3.0 + -3.0i 3.0 + -1.0i 1.0 + 3.0i 1.0 + 1.0i 1.0 + -3.0i 1.0 + -1.0i\n" +
      "-3.0 + 5.0i -1.0 + 5.0i -3.0 + -5.0i -1.0 + -5.0i -5.0 + 3.0i -5.0 + 1.0i -5.0 + -3.0i -5.0 + -1.0i -1.0 + 3.0i -1.0 + 1.0i -1.0 + -3.0i -1.0 + -1.0i -3.0 + 3.0i -3.0 + 1.0i -3.0 + -3.0i -3.0 + -1.0i 3.0 + 5.0i 1.0 + 5.0i 3.0 + -5.0i 1.0 + -5.0i 5.0 + 3.0i 5.0 + 1.0i 5.0 + -3.0i 5.0 + -1.0i 1.0 + 3.0i 1.0 + 1.0i 1.0 + -3.0i 1.0 + -1.0i 3.0 + 3.0i 3.0 + 1.0i 3.0 + -3.0i 3.0 + -1.0i\n" +
      "-7.0 + 7.0i -7.0 + 5.0i -7.0 + 1.0i -7.0 + 3.0i -7.0 + -7.0i -7.0 + -5.0i -7.0 + -1.0i -7.0 + -3.0i -5.0 + 7.0i -5.0 + 5.0i -5.0 + 1.0i -5.0 + 3.0i -5.0 + -7.0i -5.0 + -5.0i -5.0 + -1.0i -5.0 + -3.0i -1.0 + 7.0i -1.0 + 5.0i -1.0 + 1.0i -1.0 + 3.0i -1.0 + -7.0i -1.0 + -5.0i -1.0 + -1.0i -1.0 + -3.0i -3.0 + 7.0i -3.0 + 5.0i -3.0 + 1.0i -3.0 + 3.0i -3.0 + -7.0i -3.0 + -5.0i -3.0 + -1.0i -3.0 + -3.0i 7.0 + 7.0i 7.0 + 5.0i 7.0 + 1.0i 7.0 + 3.0i 7.0 + -7.0i 7.0 + -5.0i 7.0 + -1.0i 7.0 + -3.0i 5.0 + 7.0i 5.0 + 5.0i 5.0 + 1.0i 5.0 + 3.0i 5.0 + -7.0i 5.0 + -5.0i 5.0 + -1.0i 5.0 + -3.0i 1.0 + 7.0i 1.0 + 5.0i 1.0 + 1.0i 1.0 + 3.0i 1.0 + -7.0i 1.0 + -5.0i 1.0 + -1.0i 1.0 + -3.0i 3.0 + 7.0i 3.0 + 5.0i 3.0 + 1.0i 3.0 + 3.0i 3.0 + -7.0i 3.0 + -5.0i 3.0 + -1.0i 3.0 + -3.0i\n" +
      "-7.0 + 9.0i -7.0 + 11.0i -1.0 + 9.0i -1.0 + 11.0i -7.0 + -9.0i -7.0 + -11.0i -1.0 + -9.0i -1.0 + -11.0i -5.0 + 9.0i -5.0 + 11.0i -3.0 + 9.0i -3.0 + 11.0i -5.0 + -9.0i -5.0 + -11.0i -3.0 + -9.0i -3.0 + -11.0i -9.0 + 7.0i -9.0 + 5.0i -9.0 + 1.0i -9.0 + 3.0i -9.0 + -7.0i -9.0 + -5.0i -9.0 + -1.0i -9.0 + -3.0i -11.0 + 7.0i -11.0 + 5.0i -11.0 + 1.0i -11.0 + 3.0i -11.0 + -7.0i -11.0 + -5.0i -11.0 + -1.0i -11.0 + -3.0i -1.0 + 7.0i -1.0 + 5.0i -1.0 + 1.0i -1.0 + 3.0i -1.0 + -7.0i -1.0 + -5.0i -1.0 + -1.0i -1.0 + -3.0i -3.0 + 7.0i -3.0 + 5.0i -3.0 + 1.0i -3.0 + 3.0i -3.0 + -7.0i -3.0 + -5.0i -3.0 + -1.0i -3.0 + -3.0i -7.0 + 7.0i -7.0 + 5.0i -7.0 + 1.0i -7.0 + 3.0i -7.0 + -7.0i -7.0 + -5.0i -7.0 + -1.0i -7.0 + -3.0i -5.0 + 7.0i -5.0 + 5.0i -5.0 + 1.0i -5.0 + 3.0i -5.0 + -7.0i -5.0 + -5.0i -5.0 + -1.0i -5.0 + -3.0i 7.0 + 9.0i 7.0 + 11.0i 1.0 + 9.0i 1.0 + 11.0i 7.0 + -9.0i 7.0 + -11.0i 1.0 + -9.0i 1.0 + -11.0i 5.0 + 9.0i 5.0 + 11.0i 3.0 + 9.0i 3.0 + 11.0i 5.0 + -9.0i 5.0 + -11.0i 3.0 + -9.0i 3.0 + -11.0i 9.0 + 7.0i 9.0 + 5.0i 9.0 + 1.0i 9.0 + 3.0i 9.0 + -7.0i 9.0 + -5.0i 9.0 + -1.0i 9.0 + -3.0i 11.0 + 7.0i 11.0 + 5.0i 11.0 + 1.0i 11.0 + 3.0i 11.0 + -7.0i 11.0 + -5.0i 11.0 + -1.0i 11.0 + -3.0i 1.0 + 7.0i 1.0 + 5.0i 1.0 + 1.0i 1.0 + 3.0i 1.0 + -7.0i 1.0 + -5.0i 1.0 + -1.0i 1.0 + -3.0i 3.0 + 7.0i 3.0 + 5.0i 3.0 + 1.0i 3.0 + 3.0i 3.0 + -7.0i 3.0 + -5.0i 3.0 + -1.0i 3.0 + -3.0i 7.0 + 7.0i 7.0 + 5.0i 7.0 + 1.0i 7.0 + 3.0i 7.0 + -7.0i 7.0 + -5.0i 7.0 + -1.0i 7.0 + -3.0i 5.0 + 7.0i 5.0 + 5.0i 5.0 + 1.0i 5.0 + 3.0i 5.0 + -7.0i 5.0 + -5.0i 5.0 + -1.0i 5.0 + -3.0i\n" +
      "-15.0 + 15.0i -15.0 + 13.0i -15.0 + 9.0i -15.0 + 11.0i -15.0 + 1.0i -15.0 + 3.0i -15.0 + 7.0i -15.0 + 5.0i -15.0 + -15.0i -15.0 + -13.0i -15.0 + -9.0i -15.0 + -11.0i -15.0 + -1.0i -15.0 + -3.0i -15.0 + -7.0i -15.0 + -5.0i -13.0 + 15.0i -13.0 + 13.0i -13.0 + 9.0i -13.0 + 11.0i -13.0 + 1.0i -13.0 + 3.0i -13.0 + 7.0i -13.0 + 5.0i -13.0 + -15.0i -13.0 + -13.0i -13.0 + -9.0i -13.0 + -11.0i -13.0 + -1.0i -13.0 + -3.0i -13.0 + -7.0i -13.0 + -5.0i -9.0 + 15.0i -9.0 + 13.0i -9.0 + 9.0i -9.0 + 11.0i -9.0 + 1.0i -9.0 + 3.0i -9.0 + 7.0i -9.0 + 5.0i -9.0 + -15.0i -9.0 + -13.0i -9.0 + -9.0i -9.0 + -11.0i -9.0 + -1.0i -9.0 + -3.0i -9.0 + -7.0i -9.0 + -5.0i -11.0 + 15.0i -11.0 + 13.0i -11.0 + 9.0i -11.0 + 11.0i -11.0 + 1.0i -11.0 + 3.0i -11.0 + 7.0i -11.0 + 5.0i -11.0 + -15.0i -11.0 + -13.0i -11.0 + -9.0i -11.0 + -11.0i -11.0 + -1.0i -11.0 + -3.0i -11.0 + -7.0i -11.0 + -5.0i -1.0 + 15.0i -1.0 + 13.0i -1.0 + 9.0i -1.0 + 11.0i -1.0 + 1.0i -1.0 + 3.0i -1.0 + 7.0i -1.0 + 5.0i -1.0 + -15.0i -1.0 + -13.0i -1.0 + -9.0i -1.0 + -11.0i -1.0 + -1.0i -1.0 + -3.0i -1.0 + -7.0i -1.0 + -5.0i -3.0 + 15.0i -3.0 + 13.0i -3.0 + 9.0i -3.0 + 11.0i -3.0 + 1.0i -3.0 + 3.0i -3.0 + 7.0i -3.0 + 5.0i -3.0 + -15.0i -3.0 + -13.0i -3.0 + -9.0i -3.0 + -11.0i -3.0 + -1.0i -3.0 + -3.0i -3.0 + -7.0i -3.0 + -5.0i -7.0 + 15.0i -7.0 + 13.0i -7.0 + 9.0i -7.0 + 11.0i -7.0 + 1.0i -7.0 + 3.0i -7.0 + 7.0i -7.0 + 5.0i -7.0 + -15.0i -7.0 + -13.0i -7.0 + -9.0i -7.0 + -11.0i -7.0 + -1.0i -7.0 + -3.0i -7.0 + -7.0i -7.0 + -5.0i -5.0 + 15.0i -5.0 + 13.0i -5.0 + 9.0i -5.0 + 11.0i -5.0 + 1.0i -5.0 + 3.0i -5.0 + 7.0i -5.0 + 5.0i -5.0 + -15.0i -5.0 + -13.0i -5.0 + -9.0i -5.0 + -11.0i -5.0 + -1.0i -5.0 + -3.0i -5.0 + -7.0i -5.0 + -5.0i 15.0 + 15.0i 15.0 + 13.0i 15.0 + 9.0i 15.0 + 11.0i 15.0 + 1.0i 15.0 + 3.0i 15.0 + 7.0i 15.0 + 5.0i 15.0 + -15.0i 15.0 + -13.0i 15.0 + -9.0i 15.0 + -11.0i 15.0 + -1.0i 15.0 + -3.0i 15.0 + -7.0i 15.0 + -5.0i 13.0 + 15.0i 13.0 + 13.0i 13.0 + 9.0i 13.0 + 11.0i 13.0 + 1.0i 13.0 + 3.0i 13.0 + 7.0i 13.0 + 5.0i 13.0 + -15.0i 13.0 + -13.0i 13.0 + -9.0i 13.0 + -11.0i 13.0 + -1.0i 13.0 + -3.0i 13.0 + -7.0i 13.0 + -5.0i 9.0 + 15.0i 9.0 + 13.0i 9.0 + 9.0i 9.0 + 11.0i 9.0 + 1.0i 9.0 + 3.0i 9.0 + 7.0i 9.0 + 5.0i 9.0 + -15.0i 9.0 + -13.0i 9.0 + -9.0i 9.0 + -11.0i 9.0 + -1.0i 9.0 + -3.0i 9.0 + -7.0i 9.0 + -5.0i 11.0 + 15.0i 11.0 + 13.0i 11.0 + 9.0i 11.0 + 11.0i 11.0 + 1.0i 11.0 + 3.0i 11.0 + 7.0i 11.0 + 5.0i 11.0 + -15.0i 11.0 + -13.0i 11.0 + -9.0i 11.0 + -11.0i 11.0 + -1.0i 11.0 + -3.0i 11.0 + -7.0i 11.0 + -5.0i 1.0 + 15.0i 1.0 + 13.0i 1.0 + 9.0i 1.0 + 11.0i 1.0 + 1.0i 1.0 + 3.0i 1.0 + 7.0i 1.0 + 5.0i 1.0 + -15.0i 1.0 + -13.0i 1.0 + -9.0i 1.0 + -11.0i 1.0 + -1.0i 1.0 + -3.0i 1.0 + -7.0i 1.0 + -5.0i 3.0 + 15.0i 3.0 + 13.0i 3.0 + 9.0i 3.0 + 11.0i 3.0 + 1.0i 3.0 + 3.0i 3.0 + 7.0i 3.0 + 5.0i 3.0 + -15.0i 3.0 + -13.0i 3.0 + -9.0i 3.0 + -11.0i 3.0 + -1.0i 3.0 + -3.0i 3.0 + -7.0i 3.0 + -5.0i 7.0 + 15.0i 7.0 + 13.0i 7.0 + 9.0i 7.0 + 11.0i 7.0 + 1.0i 7.0 + 3.0i 7.0 + 7.0i 7.0 + 5.0i 7.0 + -15.0i 7.0 + -13.0i 7.0 + -9.0i 7.0 + -11.0i 7.0 + -1.0i 7.0 + -3.0i 7.0 + -7.0i 7.0 + -5.0i 5.0 + 15.0i 5.0 + 13.0i 5.0 + 9.0i 5.0 + 11.0i 5.0 + 1.0i 5.0 + 3.0i 5.0 + 7.0i 5.0 + 5.0i 5.0 + -15.0i 5.0 + -13.0i 5.0 + -9.0i 5.0 + -11.0i 5.0 + -1.0i 5.0 + -3.0i 5.0 + -7.0i 5.0 + -5.0i"

  val qamLUT = hardCodingSource
    .split("\n")
    .map(_.split(" ").filterNot(_ == "+").map(_.replace("i", "").toDouble))
    .map(_.grouped(2).toArray.map(pair => new Complex(pair(0), pair(1))))

  // hard-coded QAM encoding
  def qammap(input: Int, bitCount: BitCount, gray: Boolean = false) = {
    require(bitCount.value >= 1 && bitCount.value <= 8)
    require(input >= 0 && input < (1 << bitCount.value))
    qamLUT(bitCount.value - 1)(input)
  }

  def main(args: Array[String]): Unit = {
    println(qamLUT.map(_.mkString(" ")).mkString("\n"))
    println(qamLUT.map(_.length).mkString(" "))
    println(qammap(0, 4 bits))
  }
}
