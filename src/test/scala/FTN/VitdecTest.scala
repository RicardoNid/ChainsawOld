package FTN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class VitdecTest extends AnyFunSuite {

  test("testViterbi") {
    val constLen = 7
    val codeGens = Array(171, 133)
    val config = ConvencConfig(constLen, codeGens)
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
        while (true) {
          if (io.dataOut.valid.toBoolean) {
            val bit = if (io.dataOut.fragment.toBoolean) 1.0 else 0.0
            dutResult += bit
          }
          clockDomain.waitSampling()
        }
      }

      testCases.foreach { testCase =>
        peekAFlow(testCase)
        peekBubble()
        clockDomain.waitSampling(4 * tblen)
      }
      clockDomain.waitSampling()

      val trellis = MatlabRef.poly2trellis(constLen, codeGens)
      // verified by Matlab
      println(dutResult.length)
      val golden = testCases.map(testCase => MatlabRef.vitdec(testCase, trellis, tblen)).flatten
      println(dutResult.map(_.toInt).mkString(""))
      println(golden.map(_.toInt).mkString(""))
//      assert(dutResult.zip(golden).forall { case (d, d1) => d == d1 })
    }
  }
}