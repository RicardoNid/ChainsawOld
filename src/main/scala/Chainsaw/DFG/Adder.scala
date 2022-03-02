package Chainsaw.DFG

import Chainsaw._
import spinal.core._
import spinal.core.sim._

case class Adder() extends Component {
  val a, b = in UInt (4 bits)
  val c = out UInt (4 bits)

  c := a + b
}

object Adder extends App {
  GenRTL(Adder())
  SimConfig.withFstWave.compile(Adder()).doSim{dut =>
    dut.a #= 1
    dut.b #= 3
    sleep(5)
    print(dut.c.toInt)

  }
  VivadoSynth(Adder())
}
