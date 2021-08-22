package FTN

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import scala.math._
import Chainsaw._
import matlabIO._

import scala.collection.mutable.ArrayBuffer

class QammodFTNTest extends AnyFunSuite {
  test("test qammod without bitAlloc and powAlloc") {
    val pF = 256
    val bitPerSymbol = 4

    val bitAlloc = Seq.fill(pF)(bitPerSymbol)
    val powAlloc = Seq.fill(pF)(1.0)
    val period = 8

    SimConfig.withWave.compile(new QammodFTN(bitAlloc, powAlloc, period)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling()

      val dutResult = ArrayBuffer[Seq[MComplex]]()
      val monitor = fork {
        while (true) {
          dutResult += dataOut.payload.fragment.map(_.toComplex)
          clockDomain.waitSampling()
        }
      }

      val testCase = bitAlloc.map(bitAllocated => DSPRand.nextInt(1 << bitAllocated)).toArray
      val testBits = testCase.zip(bitAlloc).map { case (value, bitAllocated) =>
        value.toBinaryString.padToLeft(bitAllocated, '0')
      }.mkString("")
        .grouped(bitAlloc.sum / period).map(BigInt(_, 2)).toSeq
      printlnGreen(testBits.size)

      def testOneRound() = {
        (0 until period).foreach { i =>
          dataIn.valid #= true
          dataIn.payload.fragment #= testBits(i)
          clockDomain.waitSampling()
        }
      }

      testOneRound()
      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling(2)
      testOneRound()

      dataIn.valid #= false
      dataIn.last #= false
      clockDomain.waitSampling(period + 2)

      val golden = Communication.Refs.qammod(testCase, bitPerSymbol).map(_ / sqrt(10))

      println(golden.mkString(" "))
      println(dutResult.map(_.mkString(" ")).mkString("\n"))
      //      assert(golden.zip(yours).forall { case (c0, c1) => c0.sameAs(c1, 0.01) })
    }
  }
}
