package Chainsaw.examples

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.language.postfixOps

object AdderTree {

  // algo/software
  def adderTree(data: Seq[Int]) = data.sum

  // hardware
  case class AdderTreeHardware() extends Component {

    val dataIn = in Vec(SInt(8 bits), 8) // -128~127
    val dataOut = out SInt (11 bits)

    def add(a:SInt, b:SInt) = a +^ b
    dataOut := dataIn.reduceBalancedTree(add)
  }

  def main(args: Array[String]): Unit = {

    SimConfig.withFstWave.compile(AdderTreeHardware()).doSim { dut =>
      // design under test
      val testcases = (0 until 100).map(_ =>
        (0 until 8).map(_ => ChainsawRand.nextInt(256) - 128)
      )

      testcases.foreach { testcase =>
        dut.dataIn.zip(testcase).foreach { case (port, stimulus) => port #= stimulus }
        sleep(1)
        assert(dut.dataOut.toInt == adderTree(testcase))
      }
    }

    VivadoSynth(AdderTreeHardware())
  }
}