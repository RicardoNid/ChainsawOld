package Chainsaw.DFG

import Chainsaw._
import spinal.core._
import spinal.core.sim._

import scala.language.postfixOps

case class Adder() extends Component {

  val a, b = in UInt (4 bits)
  val c = out UInt (4 bits)
  c := RegNext(a + b)

}

object Adder extends App {

  GenRTL(Adder(), name = "Adder") // generate verilog

  SimConfig.withFstWave.compile(Adder()).doSim { dut => // simple simulation

    dut.clockDomain.forkStimulus(2)
    dut.a #= 1
    dut.b #= 3
    dut.clockDomain.waitSampling(3)
    print(s"c is ${dut.c.toInt}")
  }

  //     export DISPLAY=172.18.217.14:0.0
  //     export DISPLAY=172.19.29.33:0.0
  //     gtkwave /home/ltr/IdeaProjects/Chainsaw/simWorkspace/Adder/test.fst

  VivadoSynth(Adder())
}
