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

    SimConfig.withWave.compile(new QAMModFTN(bitAlloc, -10)).doSim { dut =>

      val bytes = Array.fill(bitAlloc.sum / 8)(1.toByte)
      DSPRand.nextBytes(bytes)
      val input = BigInt(bytes).abs

      import dut._

      dataIn #= input
      sleep(1)

      val yours = dataOut.map { bits =>
        val binaryString = bits.toBigInt.toString(2).padToLeft(2 * wordWidth, '0')
        def signedToInt(binaryString: String): Int = {
          binaryString.reverse.init.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum +
            binaryString.head.asDigit * (-1) * (1 << (binaryString.size - 1))
        }
        val real = binaryString.take(wordWidth)
        val imag = binaryString.takeRight(wordWidth)
        Seq(signedToInt(real).toDouble / (1 << -resolution), signedToInt(imag).toDouble / (1 << -resolution))
      }.flatten

      val eng = AsyncEng.get()
      val inputInts: Array[Int] = input.toString(2).padToLeft(dut.bitAlloc.sum, '0')
        .grouped(4).toArray
        .map(_.mkString("")).map(BigInt(_, 2).intValue())

      val symbols = eng.feval[Array[MComplex]]("qammod", Array.tabulate(1 << bitAllocated)(i => i), Array(1 << bitAllocated))
      val rms = eng.feval[Double]("rms", symbols)
      val golden = eng.feval[Array[MComplex]]("qammod", inputInts, Array(1 << bitAllocated))
        .map(complex => Seq(complex.real, complex.imag)).flatten.map(_ / rms)
      println(yours.mkString(" "))
      println(golden.mkString(" "))
      assert(yours.zip(golden).forall { case (d, d1) => (d - d1).abs <= (1.0 / 16) })
    }
  }
}
