package Chainsaw

import java.io._
import scala.collection.mutable.ArrayBuffer
import scala.io._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class Latency() extends Component {
  val dataIn = in(ComplexNumber(3, -5))
  val dataOut = out(ComplexNumber(3, -5))
  val delayed = true
  if (delayed) dataOut.setAsReg()
  dataOut := dataIn
  LatencyAnalysis(dataIn.real.raw, dataOut.real.raw)
}

object Main {

  def main(args: Array[String]): Unit = {

    GenRTL(new Latency())

  }
}