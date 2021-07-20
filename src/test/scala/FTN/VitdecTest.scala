package FTN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class VitdecTest extends AnyFunSuite {

  test("testViterbi") {
    val config = ConvencConfig(7, Array(171, 133))
    val tblen = config.constLen * 6
    val testCaseLen = tblen * 5

    SimConfig.withWave.compile(new Vitdec(config, tblen, debug = true)).doSim { dut =>
      import dut._
      val testCases = Seq.fill(2)(Algos.vitdecTestCase(config = config, testCaseLen, noiseNumber = 10))
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
      def peekBubble() = {
        io.dataIn.valid #= false
        io.dataIn.last #= false
      }

      // monitor
      val dutResult = ArrayBuffer[Double]()
      fork {
        if (io.dataOut.valid.toBoolean) {
          val bit = if (io.dataOut.fragment.toBoolean) 1.0 else 0.0
          dutResult += bit
        }
      }

      testCases.foreach{testCase =>
        peekAFlow(testCase)
        peekBubble()
        clockDomain.waitSampling(3 * tblen)
      }

      val golden = testCases.map(testCase => Algos.vitdec(testCase, config, tblen, verbose = false, debug = false)).flatten
      assert(dutResult.zip(golden).forall{ case (d, d1) => d == d1})
    }
  }
}
