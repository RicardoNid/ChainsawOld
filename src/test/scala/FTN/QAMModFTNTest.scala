package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import matlabIO._

import org.scalatest.funsuite.AnyFunSuite

class QAMModFTNTest extends AnyFunSuite {

  test("test qammod for FTN") {
    // 1. for fixed allocated Bits
    val parallelFactor = 128
    val bitAllocated = 4
    val bitAlloc = Array.fill(parallelFactor)(bitAllocated)

    SimConfig.withWave.compile(new QAMModFTN(bitAlloc)).doSim { dut =>

      val bytes = Array.fill(bitAlloc.sum / 8)(1.toByte)
      DSPRand.nextBytes(bytes)
      val input = BigInt(bytes).abs

      import dut._
      //      clockDomain.forkStimulus(2)
      //      clockDomain.waitSampling()

      dataIn #= input
      sleep(1)
      println(dataOut.map { bits =>
        val binaryString = bits.toBigInt.toString(2).padToLeft(12, '0')
        def signedToInt(binaryString: String): Int = {
          binaryString.reverse.init.zipWithIndex.map{ case (c, i) => c.asDigit * (1 << i)}.sum +
            binaryString.head.asDigit * (-1) * (1 << (binaryString.size - 1))
        }
        val real = binaryString.take(6)
        val imag = binaryString.takeRight(6)
        s"${signedToInt(real).toDouble / 16} ${signedToInt(imag).toDouble / 16}"
      }.mkString("\n"))

      val eng = AsyncEng.get()
      val inputInts: Array[Int] = input.toString(2).padToLeft(dut.parallelFactor,'0')
        .grouped(4).toArray
        .map(_.mkString("")).map(BigInt(_, 2).intValue())

      val golden = eng.feval[Array[MComplex]]("qammod",  inputInts, Array(1<< bitAllocated))
      println(golden.mkString(" "))

    }
  }
}
