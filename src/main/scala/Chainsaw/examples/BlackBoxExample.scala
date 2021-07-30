package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class BlackBoxExample extends BlackBox {
  val a, b = in UInt (4 bits)
  val sum = out UInt (4 bits)

  addRTLPath("adder.v")

}

class Dut extends Component{
  val a, b = in UInt (4 bits)
  val sum = out UInt (4 bits)
  val blackBoxExample = new BlackBoxExample
  blackBoxExample.a := a
  blackBoxExample.b := b
  sum := blackBoxExample.sum
}

object BlackBoxExample {
  def main(args: Array[String]): Unit = {
    SimConfig.addRtl("adder.v").withWave.compile(new Dut).doSim{ dut =>
      dut.a #= 3
      dut.b #= 1
      sleep(1)
      dut.a #= 2
      dut.b #= 2
      sleep(1)
    }
  }
}

class DSPExample extends Component{
  val io = new Bundle{
    val a = in SInt(18 bits)
    val b = in SInt(27 bits)
    val p = out SInt(45 bits)
  }
  io.p := io.a * io.b
  io.p.addAttribute("use_dsp", "yes")
}

object DSPExample extends App {
  VivadoSynth(new DSPExample)
}