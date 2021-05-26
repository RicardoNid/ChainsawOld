package FTN

import Chainsaw._
import com.mathworks.matlab.types.Complex
import spinal.core._

class Qammod(input: Bits, bitsAllocated: Int) extends DSPArea[Vec[Real], Array[Int], Array[Complex]] {
  require(input.getBitsWidth % bitsAllocated == 0)
  val symbolCount = input.getBitsWidth / bitsAllocated
  override def timing: TimingInfo = TimingInfo(1, 1, 1, 1)
  override def referenceModel(testCase: Array[Int]): Array[Complex] = {
    val bits = testCase
      .map(_.toBinaryString.reverse.padTo(bitsAllocated, '0').reverse)
      .flatten
      .map(_.asDigit)
      .map(Array(_))
    printlnWhenDebug(s"poke to matlab ${bits.map(_.mkString("")).mkString(" ")}")
    eng.putVariable("bits", bits)
    eng.eval(s"symbols = qammod(bits, 2^$bitsAllocated, 'gray', 'InputType', 'bit');")
    eng.getVariable("symbols").asInstanceOf[Array[Complex]]
  }

  override def implicitValue: Vec[Real] = {

    val bitGroups = input.subdivideIn(bitsAllocated bits)
      .reverse
    val mappers = bitGroups.map(bitGroup => Qammap(bitGroup.asUInt, bitsAllocated, resolution = 0 exp))
    val Reals = mappers.map(_.implicitValue).flatMap(complex => Array(complex.real, complex.imag))

    RegNext(Vec(Reals))
  }
}