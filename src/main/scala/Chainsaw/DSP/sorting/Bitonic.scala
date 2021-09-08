package Chainsaw.DSP.sorting

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.annotation.tailrec

/** Bitonic sorter
 *
 * learn more: https://en.wikipedia.org/wiki/Bitonic_sorter, this component implements the first pattern in wiki, try the next one!
 */
case class Bitonic[T <: Data](dataType: HardType[T], n: Int, comparator: Seq[T] => Seq[T]) extends Component {

  require(isPow2(n), "or, you should pad it")

  val dataIn = slave Flow Vec(dataType, n)
  val dataOut = master Flow Vec(dataType, n)

  private def block(dataIn: Seq[T], up: Boolean): Seq[T] = {
    val n = dataIn.length
    n match {
      case 2 => if (up) comparator(dataIn) else comparator(dataIn).reverse
      case _ => {
        val (half0, half1) = dataIn.splitAt(n / 2)
        val afterComp = half0.zip(half1).map { case (t, t1) => block(Seq(t, t1), up) }
        val ordered = Seq(0, 1).map(i => afterComp.map(_.apply(i)))
        RegNext(Vec(block(ordered(0), up) ++ block(ordered(1), up)))
      }
    }
  }

  val maxStep = n << 1

  @tailrec
  private def whole(dataIn: Seq[T], step: Int = 2): Seq[T] = {
    if (step == maxStep) dataIn
    else {
      val upAndDown = (0 until n / step).map(_ % 2 == 1)
      val ordered = dataIn.grouped(step).toSeq.zip(upAndDown).map { case (data, up) => block(data, up) }.flatten
      whole(RegNext(Vec(ordered)), step << 1)
    }
  }

  dataOut.payload := Vec(whole(dataIn.payload))
  val latency = (1 to log2Up(n)).sum
  dataOut.valid := Delay(dataIn.valid, latency, init = False)

}

object Bitonic extends App {

  def uintcomp(data: Seq[UInt]) = {
    val a = data.head
    val b = data.last
    Mux(a > b, Vec(a, b), Vec(b, a))
  }

  val size = 256

  SimConfig.withWave.compile(Bitonic(UInt(log2Up(size) bits), size, uintcomp)).doSim { dut =>
    import dut.{dataIn, dataOut, clockDomain, latency}

    clockDomain.forkStimulus(2)
    dataIn.valid #= false
    clockDomain.waitSampling()

    val testCase = (0 until size).map(_ => DSPRand.nextInt(size))
    println(testCase.mkString(" "))

    dataIn.payload.zip(testCase).foreach{ case (port, stimulus) => port #= stimulus}
    dataIn.valid #= true
    clockDomain.waitSampling()

    dataIn.valid #= false
    clockDomain.waitSampling(latency)

    val result = dataOut.payload.map(_.toInt)
    println(result.mkString(" "))
    assert(result.zip(testCase.sorted).forall{ case (i, i1) => i == i1})

  }


}
