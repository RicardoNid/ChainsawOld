package Chainsaw.dsl.dataflow

import breeze.numerics.ceil
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.immutable

import Chainsaw.waveform._


abstract class FlowDescriptor() {
  val flow: Seq[Seq[Int]]

  def period = flow.length

  def width = flow.head.length

  def data = flow.flatten.filter(_ >= 0)

  def dataCount = data.distinct.length

  def isUnique = data.length == data.distinct.length

  val charChange = (index: Int) => if (index < 0) "-" else index.toString

  def getTime(index: Int) = flow.flatten.indexWhere(_ == index) / width

  def getPort(index: Int) = flow.flatten.indexWhere(_ == index) % width

  def validCycles = flow.zipWithIndex.filter(!_._1.forall(_ == -1)).map(_._2)

  override def toString = s"data flow: \n${flow.transpose.map(_.map(charChange(_).padTo(2, ' ')).mkString(" ")).mkString("\n")}"

  def generateJsonFile() = {

    def toWave(index: Int) = if (index < -0) "x" else "="

    def toData(index: Int) = "x" + index.toString

    def addPrePost(wave: String) = "d" + wave + "d"

    val waves: Seq[String] = flow.transpose.map(seq => seq.map(toWave).mkString(""))
    val data: Seq[Seq[String]] = flow.transpose.map(seq => seq.filter(_ > -1).map(toData))
    val waveforms = waves.zip(data).zipWithIndex.map { case ((wave, data), i) => Waveform(s"port$i", addPrePost(wave), data) }

    val valid = Waveform("valid", addPrePost(flow.map(seq => if (seq.forall(_ == -1)) "0" else "1").mkString("")), Seq())
    val last = Waveform("last", addPrePost("0" * (period - 1) + "1"), Seq())

    WaveformGraph(s"flow", waveforms :+ last :+ valid).generateJsonFile()
  }
}

case class BasicFlowDescriptor(override val flow: Seq[Seq[Int]]) extends FlowDescriptor

case class PeriodicFlowDescriptor(segmentWidth: Int, step: Int, group: Int, interval: Int) extends FlowDescriptor {
  val vecWidth = (group - 1) * step + segmentWidth
  val segments: Seq[Seq[Int]] = (0 until vecWidth).sliding(segmentWidth, step).toSeq
  override val flow = segments.flatMap(data => Seq(data) ++ Seq.fill(interval - 1)(Seq.fill(segmentWidth)(-1)))

}

object FlowMethods {
  def periodicFlowAdaptor(flows: PeriodicFlowDescriptor*) = {
    require(flows.forall(_.vecWidth == flows.head.vecWidth))
    val period = flows.map(_.period).max
    flows.map { flow =>
      val original: Seq[Seq[Int]] = flow.flow
      val padded = original.padTo(period, Seq.fill(original.head.length)(-1))
      BasicFlowDescriptor(padded)
    }
  }
}

object ReuseFlowDescriptor {

  def apply(vecWidth: Int, parallelReuse: Int, step: Int, iterativeReuse: Int): PeriodicFlowDescriptor = {
    require(vecWidth > (parallelReuse - 1) * step)
    val segmentWidth = vecWidth - (parallelReuse - 1) * step
    PeriodicFlowDescriptor(segmentWidth, step, parallelReuse, iterativeReuse)
  }

  def apply(vecWidth: Int, parallelReuse: Int, iterativeReuse: Int): PeriodicFlowDescriptor = {
    require(vecWidth % parallelReuse == 0)
    apply(vecWidth, parallelReuse, vecWidth / parallelReuse, iterativeReuse)
  }

  def main(args: Array[String]): Unit = {
    val flowIn = ReuseFlowDescriptor(21, 7, 1)
    val flowOut = ReuseFlowDescriptor(21, 3, 1)
    println(flowIn)
    println(flowOut)
    val paddedFlows = FlowMethods.periodicFlowAdaptor(flowIn, flowOut)
    val Seq(flowInPadded, flowOutPadded) = paddedFlows
    flowOutPadded.generateJsonFile()
    println(paddedFlows.mkString("\n"))

    PeriodicFlowDescriptor(3, 2, 3, 2).generateJsonFile()
  }
}