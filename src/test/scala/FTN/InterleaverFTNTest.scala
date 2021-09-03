package FTN

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class InterleaverFTNTest extends AnyFunSuite {
  val row = 32
  val col = 128
  val pF = 128
  test("test interleaverFTN") {
    SimConfig.withWave.compile(InterleaverFTN(row, col, pF)).doSim { dut =>
      import dut.{dataIn, dataOut, clockDomain}

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling()

      val testCase = DSPRand.nextBinaryString(params.BitsPerFramePadded * convencConfig.m)
      val forMatlab = testCase.map(_.asDigit).toArray
      val forDut = testCase.grouped(pF).map(BigInt(_, 2)).toSeq

      val dutResult = ArrayBuffer[String]()
      val monitor = fork {
        while (true) {
          if (dataOut.valid.toBoolean) dutResult += dataOut.fragment.toBigInt.toString(2).padToLeft(pF, '0')
          clockDomain.waitSampling()
        }
      }

      def testOneFrame() = {
        forDut.indices.foreach { i =>
          dataIn.fragment #= forDut(i)
          dataIn.valid #= true
          dataIn.last #= (i == forDut.size - 1)
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
        clockDomain.waitSampling(dut.core.latency + 1)
      }

      testOneFrame()
      rest()
      testOneFrame()
      last()

      // first block
      val depth = row * col
      val golden = forMatlab.grouped(depth).map(DSP.interleave.Refs.matIntrlv(_, row, col).mkString("")).toSeq
      val yours = dutResult.grouped(depth / pF).map(_.mkString("")).toSeq

      println(golden.size)
      println(yours.size)
      println(s"${golden(0)}\n${yours(0)}")
      println(golden.zip(yours).map { case (str, str1) => str == str1 }.mkString(" "))

    }
  }
}
