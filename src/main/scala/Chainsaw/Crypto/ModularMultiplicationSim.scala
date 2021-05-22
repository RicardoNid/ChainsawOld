package Chainsaw.Crypto

import Chainsaw.{DSPSimTiming, TimingInfo, ChainsawDebug}
import spinal.core._
import spinal.core.sim._
import spinal.lib.{Delay, Flow, master, slave}

import scala.util.Random

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real




class ModularMultiplicationDUT(N: Int) extends DSPDUTTiming[Vec[UInt], UInt]{
  val innerWidth = log2Up(N - 1)
  override val input = in Vec(UInt(innerWidth bits), 2)
  val mm = new ModularMultiplication(input(0), input(1), N)
  override val output = out(mm.implicitValue)
  override val timing: TimingInfo = mm.getTimingInfo
}

/** Kernel for modular multiplication by Montgomery
 *
 * @param N modulo
 */
class ModularMultiplicationSim(N: Int) extends ModularMultiplicationDUT(N) with DSPSimTiming[Vec[UInt], UInt, (Int, Int), Int] {

  override def poke(testCase: (Int, Int), input: Vec[UInt]): Unit = {
    input(0) #= testCase._1
    input(1) #= testCase._2
    clockDomain.waitSampling()
  }

  override def peek(output: UInt): Int = {
    val ret = output.toInt
    clockDomain.waitSampling()
    ret
  }

  override def referenceModel(testCase: (Int, Int)): Int = (testCase._1 * testCase._2) % N

  override def isValid(refResult: Int, dutResult: Int): Boolean = refResult == dutResult

  override def messageWhenInvalid(testCase: (Int, Int), refResult: Int, dutResult: Int): String =
    s"\ntestCase: ${testCase}, golden: $refResult, yours: $dutResult"

  override def messageWhenValid(testCase: (Int, Int), refResult: Int, dutResult: Int): String =
    s"\ntestCase: ${testCase}, golden: $refResult, yours: $dutResult"
}


object ModularMultiplicationSim {
  def main(args: Array[String]): Unit = {
    ChainsawDebug = true
    val dut = SimConfig.withWave.compile(new ModularMultiplicationSim(133))
    dut.doSim { dut =>
      dut.sim()
      val r = new Random
      (0 until 100).foreach(_ => dut.insertTestCase(r.nextInt(133), r.nextInt(133)))
      val report = dut.simDone()
      println(report.validLog.mkString(""))
    }

    //    val report = VivadoFlow(design = new ModularMultiplicationSim(133), workspacePath = "output/MM", topModuleName = "MM").doit()
    //    report.printArea
    //    report.printFMax
  }
}



