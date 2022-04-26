package Chainsaw.dsl.dataflow

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.math.ceil

case class FlowConversion(flowIn: FlowAnalyser, flowOut: FlowAnalyser) {

  require(flowIn.dataCount == flowOut.dataCount)

  val dataCount = flowIn.dataCount

  val isUnique = flowIn.isUnique && flowOut.isUnique

  val period = flowIn.period max flowOut.period

  def tIns = (0 until dataCount).map(flowIn.getTime)

  def tZlOuts = (0 until dataCount).map(flowOut.getTime)

  def tDiff = tZlOuts.zip(tIns).map { case (a, b) => a - b }

  def latency = tDiff.min.abs

  def tOuts = tZlOuts.map(_ + latency)

  def lifeCycles = tOuts.zip(tIns).map { case (a, b) => a - b }

  def positive(value: Double) = if (value > 0) value else 0

  def dataAlive(time: Int) =
    tIns.map(tIn => ceil(positive(time - tIn.toDouble) / period)).sum.toInt -
      tOuts.map(tOut => ceil(positive(time - tOut.toDouble) / period)).sum.toInt

  def minimizedRegisterCount = (latency until latency + period).map(dataAlive).max

  def toKaTex = {
    val head = "\\begin{array}{crrrrrc}"
    val last = "\\hline\\end{array}"
    val header = "\\hline \\text { Variable }(v) & T_{\\text {input }} & T_{\\text {zlout }} & T_{\\text {diff }} & T_{\\text {out }} & L(v) & \\text { Life Period } \\\\ \\hline"
    val contents = (0 until dataCount).map(index => s"\\text { $index } & ${tIns(index)} & ${tZlOuts(0)} & ${tDiff(index)} & ${tOuts(index)} & ${tOuts(index) - tIns(index)} & ${tIns(index)} \\rightarrow ${tOuts(index)} \\\\").mkString("\n")
    Seq(head, header, contents, last).mkString(" ")
  }
}

object FlowConversion {
  def main(args: Array[String]): Unit = {
//
//    val flowIn = BasicFlow((0 until 16).map(Seq(_)))
//    val flowOut = BasicFlow(Seq(0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15).map(Seq(_)))
//    val intrlv = FlowConversion(flowIn, flowOut)
//
//    val serialIn = PeriodicFlow(1, 1, 4, 1)
//    val parallelOut = PeriodicFlow(4, 4, 1, 4)
//    val s2p = FlowConversion(serialIn, parallelOut)
//
//    println(s2p.toKaTex)
//    println(new ForwardRegisterAllocator(s2p).getAllocation)
//    GenRTL(FlowConverter(s2p, new ForwardRegisterAllocator(s2p).getAllocation, UInt(8 bits)))
//    SimConfig.withFstWave.compile(FlowConverter(s2p, new ForwardRegisterAllocator(s2p).getAllocation, UInt(8 bits))).doSim { dut =>
//      dut.clockDomain.forkStimulus(2)
//      dut.dataIn.last #= true
//      dut.clockDomain.waitSampling()
//      dut.dataIn.last #= false
//      (0 until 8).foreach { data =>
//        dut.dataIn.payload(0) #= data % 4
//        if (data % 4 == 3) dut.dataIn.last #= true
//        else dut.dataIn.last #= false
//        dut.clockDomain.waitSampling()
//      }
//    }
  }
}