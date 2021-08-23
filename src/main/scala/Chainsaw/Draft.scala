package Chainsaw

import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import Chainsaw.matlabIO._

case class ComplexMult(pipeline:Int = 0) extends Component {
    val dataIn = in(Vec(ComplexNumber(7, -8), 2))
    val dataOut = out(ComplexNumber(7, -8))
    implicit val complexMultDelay = pipeline

    dataOut := (dataIn(0) * dataIn(1)).truncated(HardType(dataOut.real))
    println(LatencyAnalysis(dataIn(0).real.raw, dataOut.real.raw).intValue())
}

object Gen {
  def main(args: Array[String]): Unit = {
    (0 until 4).foreach(i => GenRTL(ComplexMult(i)))
  }
}

object Main {

  def main(args: Array[String]): Unit = {

//    def testComplexMult(pipeline:Int) = {
//      SimConfig.withWave.compile(ComplexMult(pipeline)).doSim{ dut =>
//        val a = new MComplex(6.15, 4.18)
//        val b = new MComplex(3.33, 2.22)
//
//        dut.clockDomain.forkStimulus(2)
//        dut.clockDomain.waitSampling()
//        dut.dataIn(0).real #= a.real
//        dut.dataIn(0).imag #= a.imag
//        dut.dataIn(1).real #= b.real
//        dut.dataIn(1).imag #= b.imag
//        dut.clockDomain.waitSampling(pipeline + 1)
//
//        println(a * b)
//        println(dut.dataOut.real.toDouble)
//        println(dut.dataOut.imag.toDouble)
//      }
//    }
//
//    (1 until 4).foreach(testComplexMult(_))

    VivadoSynth(ComplexMult())
//    VivadoSynth(ComplexMult(1))
//    VivadoSynth(ComplexMult(2))
//    VivadoSynth(ComplexMult(3))
  }
}