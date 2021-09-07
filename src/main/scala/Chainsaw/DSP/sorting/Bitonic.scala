package Chainsaw.DSP.sorting

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import matlabIO._

import scala.annotation.tailrec

case class Bitonic[T <: Data](dataType: HardType[T], n: Int, comparator: Seq[T] => Seq[T]) extends Component {

  require(isPow2(n), "or, you should pad it")

  val dataIn = slave Flow Vec(dataType, n)
  val dataOut = master Flow Vec(dataType, n)

  def block(dataIn: Seq[T], up: Boolean): Seq[T] = {
    val n = dataIn.length
    n match {
      case 2 => if (up) comparator(dataIn) else comparator(dataIn).reverse
      case _ => {
        val half0 = dataIn.take(n / 2)
        val half1 = dataIn.takeRight(n / 2)
        val afterComp = half0.zip(half1).map { case (t, t1) => block(Seq(t, t1), up) }
        val ordered0 = afterComp.map(_.head)
        val ordered1 = afterComp.map(_.last)
        RegNext(Vec(block(ordered0, up) ++ block(ordered1, up)))
      }
    }
  }

  val maxStep = n << 1

  def whole(dataIn: Seq[T], step: Int = 2): Seq[T] = {
    if (step == maxStep) dataIn
    else {
      val upAndDown = (0 until n / step).map(_ % 2 == 1)
      println(s"step  = $step")
      println(s"up and down ${upAndDown.mkString(" ")}")
      val ordered = dataIn.grouped(step).toSeq.zip(upAndDown).map { case (data, up) => block(data, up) }.flatten
      val delayed = RegNext(Vec(ordered))
      whole(delayed, step << 1)
    }
  }

  dataOut.payload := Vec(whole(dataIn.payload))
  val latency = log2Up(n) * (log2Up(n) + 1) / 2
  println(s"latency = $latency")
  dataOut.valid := Delay(dataIn.valid, latency, init = False)

}

object Bitonic extends App {

  def uintcomp(data:Seq[UInt]) = {
    val a = data.head
    val b = data.last
    Mux(a > b, Vec(a, b), Vec(b, a))
  }

  val size = 16

  SimConfig.withWave.compile(Bitonic(HardType(UInt(4 bits)), size, uintcomp)).doSim{dut =>
    import dut.{clockDomain, dataIn, dataOut, latency}

    clockDomain.forkStimulus(2)
    dataIn.valid #= false
    clockDomain.waitSampling()

    val reversed = (0 until size).reverse

    dataIn.payload.zip(reversed).foreach{ case (int, i) => int #= i}
    dataIn.valid #= true
    clockDomain.waitSampling()

    dataIn.valid #= false
    clockDomain.waitSampling(latency + 10)

    println(dataOut.payload.map(_.toInt).mkString(" "))

  }
}
