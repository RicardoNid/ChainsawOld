package FTN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
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
    val testCaseLen = tblen * 5

    SimConfig.withWave.compile(new Vitdec(config, tblen, debug = true)).doSim { dut =>
      import dut._
      val testcase = Algos.vitdecTestCase(config = config, testCaseLen, noiseNumber = 10)
      clockDomain.forkStimulus(2)
      io.dataIn.valid #= false
      io.dataIn.last #= false
      clockDomain.waitSampling(2)

      def peekAFlow(testcase: Array[Double]) = {
        val frames: Seq[BigInt] = testcase.map(_.toInt).grouped(config.m).toSeq.map(bits => BigInt(bits.mkString(""), 2))
        frames.indices.foreach { i =>
          io.dataIn.valid #= true
          io.dataIn.fragment #= frames(i)
          io.dataIn.last #= i == frames.length - 1
          clockDomain.waitSampling()
        }
      }

      peekAFlow(testcase)
      peekAFlow(testcase)

      Algos.vitdec(testcase, config, tblen, verbose = false, debug = true)
    }
  }
}
