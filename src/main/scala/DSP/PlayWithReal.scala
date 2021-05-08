package DSP

import spinal.core._
import spinal.core.sim._

import scala.util.Random

class PlayWithReal extends Component {


  val a = SReal(IntRange(0, 7))
  val b = SReal(IntRange(0, 11))
  val c = SReal(IntRange(0, 20))
  c := a + b

  val d = SReal(5 exp, -5 exp)
  val e = SReal(2 exp, -2 exp)
  val f = SReal(7 exp, 4 exp)
  val g = SReal(-4 exp, -7 exp)

  val contains = d + e
  val overlap0 = d + f
  val overlap1 = d + g
  val noIntersaction0 = e + f
  val noIntersaction1 = e + g

  in(a, b, d, e, f, g)
  out(c, contains, overlap0, overlap1, noIntersaction0, noIntersaction1)

}

class PlayWithRealSim extends PlayWithReal {
  implicit class myBinaryString(val bs: String) {
    /** Allocate bits to multiple elements
     */
    def =<<(elems: SReal*) = {
      val prefix = (0 to elems.length).map(i => elems.map(_.getBitsWidth).take(i).sum)
      val intervals = prefix.dropRight(1).zip(prefix.drop(1))
      val slices = intervals.map { case (start, end) => bs.slice(start, end) }
      elems.zip(slices).foreach { case (elem, slice) => elem #= bs2i2c(slice) }
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

  def traversalTestAddtion(outputs: IndexedSeq[SReal], inputs: IndexedSeq[SReal]) = {
    require(inputs.forall(real => real.raw.getBitsWidth == real.range.range.getMaxExp - real.range.range.getMinExp + 1))
    //    require(outputs.forall(real => real.raw.getBitsWidth == real.range.range.getMaxExp - real.range.range.getMinExp + 1))

    val lengths = inputs.map(_.raw.getBitsWidth)
    val caseNum = 1 << lengths.sum
    println(s"$caseNum cases would be tested")
    allBits(lengths.sum).map { bs =>
      bs =<< (inputs: _*)
      sleep(1)
      if (inputs.map(_.raw.toInt).sum != outputs(0).raw.toInt) println(s"${inputs.map(_.raw.toInt).mkString(" ")} ${outputs.map(_.raw.toInt).mkString(" ")}")
    }
  }
}

object PlayWithReal {

  val r = new Random()

  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new PlayWithFix)
    SimConfig.compile(new PlayWithRealSim).doSim { dut =>
      dut.traversalTestAddtion(IndexedSeq(dut.c), IndexedSeq(dut.a, dut.b))
      //      dut.traversalTestAddtion(IndexedSeq(dut.contains), IndexedSeq(dut.d, dut.e))
      //      dut.traversalTestAddtion(IndexedSeq(dut.overlap0), IndexedSeq(dut.d, dut.f))
      //      dut.traversalTestAddtion(IndexedSeq(dut.overlap1), IndexedSeq(dut.d, dut.g))
      //      dut.traversalTestAddtion(IndexedSeq(dut.noIntersaction0), IndexedSeq(dut.e, dut.f))
      //      dut.traversalTestAddtion(IndexedSeq(dut.noIntersaction1), IndexedSeq(dut.e, dut.g))
    }
  }
}

