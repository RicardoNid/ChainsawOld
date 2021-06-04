package FTN

import Chainsaw._
import com.mathworks.matlab.types.Complex
import spinal.core._
import spinal.lib._

class DynamicQammod(input: Bits, bitsAllocated: Array[Int]) extends DSPArea[Vec[Real], Array[Int], Array[Complex]] {
  // caution: bitAllocated can be 0
  require(input.getBitsWidth == bitsAllocated.sum)
  override def timing: TimingInfo = TimingInfo(1, 1, 1, 1)

  override def referenceModel(testCase: Array[Int]): Array[Complex] = {
    val bits = testCase.zip(bitsAllocated)
      .map { case (number, bitAllocated) => number.toBinaryString.reverse.padTo(bitAllocated, '0').reverse }
      .flatten
      .map(_.asDigit)
      .map(Array(_))
    printlnWhenDebug(s"poke to matlab ${bits.map(_.mkString("")).mkString(" ")}")
    eng.eval("cd /home/ltr/IdeaProjects/Chainsaw/src/main/scala/FTN/Matlab")
    eng.putVariable("bits", bits)
    eng.putVariable("bitAlloc", bitsAllocated.map(_.toDouble))
    eng.eval(s"symbols = DynamicQammodSimplified(bits, bitAlloc)")
    eng.getVariable("symbols").asInstanceOf[Array[Complex]]
  }

  override def implicitValue: Vec[Real] = {

    val start = bitsAllocated.indices.map(i => bitsAllocated.take(i).sum)
    val end = start.tail :+ bitsAllocated.sum
    printlnWhenDebug(s"start: ${start.mkString(" ")}")
    printlnWhenDebug(s"end: ${end.mkString(" ")}")
    val bitGroups = bitsAllocated.indices.map(i => input.asBools.reverse.slice(start(i), end(i)).asBits().reversed)
    bitGroups.indices.foreach(i => bitGroups(i).setName(s"bitGroup_$i"))

    val mappers = bitGroups.map(bitGroup => Qammap(bitGroup.asUInt, bitGroup.getBitsWidth, resolution = 0 exp))
    val Reals = mappers.map(_.implicitValue).flatMap(complex => Array(complex.real, complex.imag))

    RegNext(Vec(Reals))
  }
}

object DynamicQammod {
  def apply(input: Bits, bitsAllocated: Array[Int]): DynamicQammod = new DynamicQammod(input, bitsAllocated)
}