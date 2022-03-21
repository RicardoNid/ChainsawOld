package Chainsaw.comparith

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class SignedCarryExample extends Component {
  val x, y = in SInt (4 bits)
  val ret  = out(x +^ y)
}

object SignedCarryExample {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new SignedCarryExample).doSim { dut =>
      import dut._
      def checkOnce() = {
        x.randomize()
        y.randomize()
        if (ret.toBigInt != x.toBigInt + y.toBigInt) println(Seq(x, y, ret).map(_.toBigInt).mkString(" "))
        sleep(1)
      }
      (0 until 100).foreach(_ => checkOnce())
    }
  }
}
