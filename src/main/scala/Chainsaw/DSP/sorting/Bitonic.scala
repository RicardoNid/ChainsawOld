package Chainsaw.DSP.sorting

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/** Bitonic sorter
 *
 * learn more: [[https://en.wikipedia.org/wiki/Bitonic_sorter]]
 */
case class Bitonic[T <: Data](dataType: HardType[T], n: Int,
                              comparator: Seq[T] => Seq[T],
                              pattern1: Boolean = true) extends Component {

  require(isPow2(n), "or, you should pad it")

  val dataIn = slave Flow Vec(dataType, n)
  val dataOut = master Flow Vec(dataType, n)

  def cmp(dataIn: Seq[T], up: Boolean) = if (up && pattern1) comparator(dataIn) else comparator(dataIn).reverse

  def subBlock(dataIn: Seq[T], up: Boolean, red: Boolean) = {
    val n = dataIn.length
    val (half0, half1) = dataIn.splitAt(n / 2)

    val afterComp = if (red) half0.zip(half1).map { case (t, t1) => cmp(Seq(t, t1), up) }
    else half0.zip(half1.reverse).map { case (t, t1) => cmp(Seq(t, t1), up) }

    val ordered = Seq(0, 1).map(i => afterComp.map(_.apply(i)))

    if (red) (ordered(0), ordered(1)) else (ordered(0), ordered(1).reverse)
  }


  private def block(dataIn: Seq[T], up: Boolean, first: Boolean): Seq[T] = {
    val n = dataIn.length
    n match {
      case 2 => cmp(dataIn, up)
      case _ =>
        val compared = if (first) subBlock(dataIn, up, pattern1) else subBlock(dataIn, up, true)
        RegNext(Vec(block(compared._1, up, false) ++ block(compared._2, up, false)))
    }
  }

  val maxStep = n << 1

  @tailrec
  private def whole(dataIn: Seq[T], step: Int = 2): Seq[T] = {
    if (step == maxStep) dataIn
    else {
      val upAndDown = (0 until n / step).map(_ % 2 == 1)
      val ordered = dataIn.grouped(step).toSeq.zip(upAndDown).map { case (data, up) => block(data, up, true) }.flatten
      whole(RegNext(Vec(ordered)), step << 1)
    }
  }

  dataOut.payload := Vec(whole(dataIn.payload))
  val latency = (1 to log2Up(n)).sum
  dataOut.valid := Delay(dataIn.valid, latency, init = False)

}

object Bitonic extends App {

  def uintcomp(dataIn: Seq[UInt]) = {
    require(dataIn.size == 2)
    val a = dataIn.head
    val b = dataIn.last
    Mux(a > b, Vec(a, b), Vec(b, a))
  }

  val size = 16

  SimConfig.withWave.compile(Bitonic(UInt(log2Up(size) bits), size, uintcomp, false)).doSim { dut =>
    import dut.{dataIn, dataOut, clockDomain, latency}

    val dutResult = ArrayBuffer[BigInt]()
    dataIn.halt()
    dataOut.setMonitor(dutResult)
    clockDomain.forkStimulus(2)
    clockDomain.waitSampling()

    val testCase: Seq[BigInt] = (0 until size).map(_ => ChainsawRand.nextInt(size)).map(BigInt(_))
    dataIn.poke(testCase)
    clockDomain.waitSampling(latency + 1)

    println(s"before sorting: ${testCase.mkString(" ")}")
    println(s"after sorting:  ${dutResult.mkString(" ")}")
    //    assert(dutResult.zip(testCase.sorted).forall { case (i, i1) => i == i1 })
  }
}
