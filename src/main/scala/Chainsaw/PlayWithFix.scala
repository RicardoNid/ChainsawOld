package Chainsaw

import spinal.core._
import spinal.core.sim._ //  for digital signal processing

class PlayWithFix extends Component {
  val output = out UFix(20 exp, -5 exp)
  val input1 = in UFix(3 exp, -3 exp)

  //  val input0 = in SFix(5 exp, -5 exp)
  //  output := input1 + (input0 << 10)

  val input0 = in UFix(15 exp, 5 exp)
  output := input1 + input0
  //  val input0 = in SInt (20 bits)
  //  val input1 = in SInt (20 bits)
  //  val output = out SInt (20 bits)
  //
  //  output := RegNext(RegNext(input0) / RegNext(input1))
  val a = SInt(4 bits)
  val b = SInt(4 bits)
  val c = a + b
  val d = a +^ b
  val e = SInt(4 bits)
  e := (a +^ b).resized
  in(a, b)
  out(c, d, e)

}

class PlayWithFixSim extends PlayWithFix {

  implicit class myBinaryString(val bs: String) {
    /** Allocate bits to multiple elements
     */
    def =<<[T <: BitVector](elems: T*) = {
      val prefix = (0 to elems.length).map(i => elems.map(_.getBitsWidth).take(i).sum)
      val intervals = prefix.dropRight(1).zip(prefix.drop(1))
      val slices = intervals.map { case (start, end) => bs.slice(start, end) }
      if (elems.isInstanceOf[Seq[SInt]]) elems.zip(slices).foreach { case (elem, slice) => elem #= bs2i2c(slice) }
      else elems.zip(slices).foreach { case (elem, slice) => elem #= bs2i(slice) }
    }
  }

  def bs2i(bs: String) = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum
  def bs2i2c(bs: String) = {
    val values = bs.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }
    values.dropRight(1).sum - values.last
  }

  def allBits(length: Int) = {
    val bs = (0 until (1 << length)).map(_.toBinaryString)
    bs.map(s => "0" * (length - s.length) + s)
  }

  def traversalTest[T <: BitVector](outputs: IndexedSeq[T], inputs: IndexedSeq[T]) = {
    val lengths = inputs.map(_.getBitsWidth)
    val caseNum = 1 << lengths.sum
    println(s"$caseNum cases would be tested")
    allBits(lengths.sum).map { bs =>
      bs =<< (inputs: _*)
      sleep(1)
      println(s"${inputs.map(_.toInt).mkString(" ")} ${outputs.map(_.toInt).mkString(" ")}")
      assert(inputs.map(_.toInt).sum == outputs(0).toInt)
    }
  }

}

object PlayWithFix {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new PlayWithFix)
    //    val report = VivadoFlow(new PlayWithFix, "Another", "output/Another", force = true).doit()
    //    report.printFMax
    //    report.printArea
    SimConfig.compile(new PlayWithFixSim).doSim { dut =>

      def addAssertion[T <: BitVector](inputs: IndexedSeq[T], outputs: IndexedSeq[T]) =
        outputs(0).toInt == inputs(0).toInt + inputs(1).toInt

      dut.traversalTest(IndexedSeq(dut.d), IndexedSeq(dut.a, dut.b))
    }
  }
}