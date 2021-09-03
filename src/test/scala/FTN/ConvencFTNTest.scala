package FTN

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class ConvencFTNTest extends AnyFunSuite {

  test("testConvenc for FTN") {
    val config = ConvencConfig(7, Array(171, 133))
    val pF = 128
    SimConfig.withWave.compile(ConvencFTN(config, pF)).doSim { dut =>
      import dut.{dataIn, dataOut, clockDomain}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling()

      val dutResult = ArrayBuffer[String]()
      val monitor = fork {
        while (true) {
          if (dataOut.valid.toBoolean) dutResult += dataOut.fragment.toBigInt.toString(2).padToLeft(pF * config.m, '0')
          clockDomain.waitSampling()
        }
      }

      val testCase = DSPRand.nextBinaryString(params.BitsPerFramePadded)
      val forMatlab = testCase.map(_.asDigit.toDouble).toArray
      val forDut = testCase.grouped(pF).map(BigInt(_, 2)).toSeq

      def testOneFrame() = {
        forDut.indices.foreach { i =>
          dataIn.valid #= true
          dataIn.last #= (i == forDut.size - 1)
          dataIn.fragment #= forDut(i)
          clockDomain.waitSampling()
        }
      }

      def rest() = {
        dataIn.valid #= false
        dataIn.last #= false
        clockDomain.waitSampling(2)
      }

      def last() = {
        dataIn.valid #= false
        dataIn.last #= false
        clockDomain.waitSampling(dut.latency + 1)
      }

      testOneFrame()
      rest()
      testOneFrame()
      last()

      val trellis = MatlabRef.poly2trellis(dut.config.K, dut.config.codeGens)
      val oneFrame = MatlabRef.convenc(forMatlab, trellis).map(_.toInt).mkString("")
      val golden = oneFrame + oneFrame
      println(golden.size)
      val yours = dutResult.mkString("")
      println(yours.size)

      assertResult(expected = golden)(actual = yours)
      printlnGreen("first cycle of I/O ")
      println(golden.take(pF * config.m).mkString(""))
      println(dutResult(0))
    }
  }

}
