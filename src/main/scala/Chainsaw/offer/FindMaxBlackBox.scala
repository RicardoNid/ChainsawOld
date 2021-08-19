package Chainsaw.offer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class FindMaxBlackBox() extends BlackBox {

  val data_in = in Bits (1024 bits)
  val data_out = out UInt (32 bits)

  setDefinitionName("FindMax32")
  addRTLPath("/home/ltr/IdeaProjects/Chainsaw/FindMax32.sv")
}

case class FindMaxPipelinedBlackBox() extends BlackBox {

  val clk, rst, valid_in = in Bool()
  val valid_out = out Bool()
  val data_in = in Bits (1024 bits)
  val data_out = out UInt (32 bits)

  setDefinitionName("FindMax32Pipelined")
  addRTLPath("/home/ltr/IdeaProjects/Chainsaw/FindMax32Pipelined.sv")

  mapCurrentClockDomain(clock = clk, reset = rst)
}

case class FindMaxDUT() extends Component {

  val dut = FindMaxBlackBox()
  val data_in = in Bits (1024 bits)
  val data_out = out UInt (32 bits)

  val data_in_vec = data_in.subdivideIn(32 slices)
  data_in_vec.simPublic()

  dut.data_in := data_in
  data_out := dut.data_out
}

case class FindMaxPipelinedDUT() extends Component {

  val dut = FindMaxPipelinedBlackBox()
  val valid_in = in Bool()
  val valid_out = out Bool()
  val data_in = in Bits (1024 bits)
  val data_out = out UInt (32 bits)

  val data_in_vec = data_in.subdivideIn(32 slices)
  data_in_vec.simPublic()

  dut.valid_in := valid_in
  dut.data_in := data_in
  data_out := dut.data_out
  valid_out := dut.valid_out
}

object FindMaxBlackBox extends App {

//  SimConfig.addRtl("FindMax32.sv").withWave.compile(new FindMaxDUT).doSim { dut =>
//    val bytes = Array.fill(128)(0.toByte)
//    def testOnce() = {
//      DSPRand.nextBytes(bytes)
//      dut.data_in #= BigInt(bytes).abs
//      sleep(1)
//    }
//    testOnce()
//    testOnce()
//    testOnce()
//    testOnce()
//  }

//    SimConfig.addRtl("/home/ltr/IdeaProjects/Chainsaw/FindMax32Pipelined.sv").withWave.compile(new FindMaxPipelinedDUT).doSim { dut =>
//
//      dut.clockDomain.forkStimulus(2)
//      dut.valid_in #= false
//      dut.clockDomain.waitSampling()
//      dut.clockDomain.assertReset()
//      sleep(4)
//      dut.clockDomain.deassertReset()
//      dut.clockDomain.waitSampling()
//
//
//      val bytes = Array.fill(128)(0.toByte)
//      def testOnce() = {
//        DSPRand.nextBytes(bytes)
//        dut.valid_in #= true
//        dut.data_in #= BigInt(bytes).abs
//        dut.clockDomain.waitSampling()
//      }
//      testOnce()
//      testOnce()
//      testOnce()
//      testOnce()
//
//      dut.valid_in #= false
//      dut.clockDomain.waitSampling(10)
//    }

  VivadoSynth(new FindMaxDUT)
  VivadoSynth(new FindMaxPipelinedDUT)
}
