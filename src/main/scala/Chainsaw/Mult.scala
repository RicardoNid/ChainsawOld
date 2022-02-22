package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class Mult() extends Component {
  val a, b = in SFix(3 exp, -14 exp)
  val c = out(a * b)
  println(c.maxExp, c.minExp)
}

object Mult extends App {
  GenRTL(Mult(), "mult")
  SpinalSimConfig().withFstWave.compile(Mult()).doSim { dut =>
    dut.a #= 1.23
    dut.b #= -3.23
    sleep(2)
    println(dut.c.toDouble)
  }
  VivadoSynth(Mult())
  VivadoImpl(Mult())
}
