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


abstract class FlowAnalyser() {
  val flow: Seq[Seq[Int]]

  def period = flow.length

  def width = flow.head.length

  def data = flow.flatten.filter(_ >= 0)

  def dataCount = data.distinct.length

  def isUnique = data.length == data.distinct.length

  def getTime(index: Int) = flow.flatten.indexWhere(_ == index) / width

  def getPort(index: Int) = flow.flatten.indexWhere(_ == index) % width

  def validCycles = flow.zipWithIndex.filter(!_._1.forall(_ == -1)).map(_._2)

  val charChange = (index: Int) => if (index < 0) "-" else index.toString

  override def toString = s"data flow: \n${flow.transpose.map(_.map(charChange(_).padTo(2, ' ')).mkString(" ")).mkString("\n")}"

  def generateJsonFile(name:String, symbol:String) = {

    def toWave(index: Int) = if (index < -0) "x" else "="

    def toData(index: Int) = symbol + index.toString

    def addPrePost(wave: String) = "d" + wave + "d"

    val waves: Seq[String] = flow.transpose.map(seq => seq.map(toWave).mkString(""))
    val data: Seq[Seq[String]] = flow.transpose.map(seq => seq.filter(_ > -1).map(toData))
    val waveforms = waves.zip(data).zipWithIndex.map { case ((wave, data), i) => Waveform(s"port$i", addPrePost(wave), data) }

    val valid = Waveform("valid", addPrePost(flow.map(seq => if (seq.forall(_ == -1)) "0" else "1").mkString("")), Seq())
    val last = Waveform("last", addPrePost("0" * (period - 1) + "1"), Seq())

    WaveformGraph(name, waveforms :+ last :+ valid).generateJsonFile()
  }
}

case class BasicFlowAnalyser(override val flow: Seq[Seq[Int]]) extends FlowAnalyser