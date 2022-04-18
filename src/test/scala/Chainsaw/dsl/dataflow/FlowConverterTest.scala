//package Chainsaw.dsl.dataflow
//
//import org.scalatest.flatspec.AnyFlatSpec
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.lib.fsm._
//
//import Chainsaw._
//import Chainsaw.matlabIO._
//import Chainsaw.dspTest._
//
//class FlowConverterTest extends AnyFlatSpec {
//
//  val flowIn = FlowDescriptor(Seq(0, -1, 1, -1, 2, 3, 4, 5, -1), 3)
//  val flowOut = FlowDescriptor(Seq(2, 3, 4, 5, 0, 1), 2)
//
////  SimConfig.withFstWave.compile(FlowConverter(flowIn, flowOut, HardType(UInt(4 bits)))).doSim { dut =>
////
////    dut.clockDomain.forkStimulus(2)
////    dut.clockDomain.waitSampling()
////    dut.dataIn.last #= true // as a reset
////    dut.clockDomain.waitSampling()
////
////    (0 until dut.period).foreach { i =>
////
////      if (i < flowIn.map.length) flowIn.map(i).zip(dut.dataIn).foreach { case (data, port) => if (data >= 0) port #= data else port #= ChainsawRand.nextInt(16) }
////      if (i == dut.period - 1) dut.dataIn.last #= true else dut.dataIn.last #= false
////      dut.clockDomain.waitSampling()
////    }
////
////    dut.dataIn.last #= false
////    dut.clockDomain.waitSampling(3)
////  }
////
//  VivadoImpl(FlowConverter(flowIn, flowOut, HardType(UInt(4 bits))))
//
//
//}
