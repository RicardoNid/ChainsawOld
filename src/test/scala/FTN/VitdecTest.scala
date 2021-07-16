package FTN

import org.scalatest.funsuite.AnyFunSuite

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class VitdecTest extends AnyFunSuite {

  test("testViterbi") {
    val config = ConvencConfig(7, Array(171, 133))
    val tblen = config.constLen * 6
    SimConfig.withWave.compile(new Vitdec(config, tblen)).doSim { dut =>
      import dut._
      val testcase = Algos.vitdecTestCase(config = config, tblen * 5, noiseNumber = 10)
      clockDomain.forkStimulus(2)
      val frames: Seq[BigInt] = testcase.map(_.toInt).grouped(config.m).toSeq.map(bits => BigInt(bits.mkString(""), 2))
      frames.foreach { frame =>
        io.frame #= frame
        clockDomain.waitSampling()
      }
      Algos.vitdec(testcase, config, tblen, verbose = false, debug = true)
    }
  }
}
