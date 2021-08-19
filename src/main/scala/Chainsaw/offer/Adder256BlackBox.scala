package Chainsaw.offer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class Adder256BlackBox() extends BlackBox {

  val clk = in Bool()
  val rst = in Bool()

  val io = new Bundle {
    val valid_in, last_in = in Bool()
    val valid_out, last_out = out Bool()
    val data_in_0, data_in_1 = in UInt (8 bits)
    val data_out = out UInt (8 bits)
  }

  addRTLPath("/home/ltr/IdeaProjects/Chainsaw/Adder256.sv")
  setDefinitionName("Adder256")
  mapCurrentClockDomain(clock = clk, reset = rst)
  noIoPrefix()

}

case class Adder256DUT() extends Component {

  val dut = Adder256BlackBox()
  val io = new Bundle {
    val valid_in, last_in = in Bool()
    val valid_out, last_out = out Bool()
    val data_in_0, data_in_1 = in UInt (8 bits)
    val data_out = out UInt (8 bits)
  }

  dut.io <> io
}

object Adder256DUT extends App {
//  VivadoSynth(new Adder256DUT)
  SimConfig.addRtl("/home/ltr/IdeaProjects/Chainsaw/Adder256.sv").withWave.compile(new Adder256DUT()).doSim{dut =>

    dut.clockDomain.doStimulus(2)
    dut.clockDomain.waitSampling()
    dut.io.valid_in #= false
    dut.io.last_in #= false
    dut.clockDomain.assertReset()
    sleep(4)
    dut.clockDomain.deassertReset()
    dut.clockDomain.waitSampling()

    val bytes = Array.fill(32)(0.toByte)
    def nextBigInt() = {
      DSPRand.nextBytes(bytes)
      BigInt(bytes).abs
    }
    val op0 = nextBigInt()
    val op1 = nextBigInt()

    def byte2unsigned(bt:Byte): Int = bt.toBinaryString.padToLeft(8,'0').takeRight(8).reverse.zipWithIndex.map{ case (c, i) =>  c.asDigit * (1 << i)}.sum

    (0 until 32).foreach{ i =>
      dut.io.data_in_0 #= byte2unsigned(op0.toByteArray(31 - i))
      dut.io.data_in_1 #= byte2unsigned(op1.toByteArray(31 - i))
      dut.io.valid_in #= true
      dut.io.last_in #= (i == 31)
      dut.clockDomain.waitSampling()
    }

    dut.io.valid_in #= false
    dut.io.last_in #= false
    dut.clockDomain.waitSampling(10)

    println((op0 + op1).showWordsHex(8).mkString(" "))
  }
}


