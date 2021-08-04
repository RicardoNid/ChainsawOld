package FTN


import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class InterleaverTest extends AnyFunSuite {
  test("testInterleaver for FTN") {
    SimConfig.withWave.compile(new Interleaver(16, 16, 32)).doSim { dut =>
      import dut._

      val eng = AsyncEng.get()
      clockDomain.forkStimulus(2)

      running #= false
      we #= false
      clockDomain.waitSampling()

      val inputBlock = ArrayBuffer[Int]()
      val outputBlock = ArrayBuffer[Int]()

      val latency = row * col / parallelFactor
      (0 until latency).foreach { i =>
        running #= true
        we #= true
        val bytes = Array.fill(parallelFactor / 8)(1.toByte)
        DSPRand.nextBytes(bytes)
        val input = BigInt(bytes).abs
        dataIn #= input
        inputBlock ++= input.toString(2).padToLeft(parallelFactor, '0').map(_.asDigit)

        clockDomain.waitSampling()
      }

      (0 until latency).foreach { i =>
        running #= true
        we #= false
        clockDomain.waitSampling()
        outputBlock ++= dataOut.toBigInt.toString(2).padToLeft(parallelFactor, '0').map(_.asDigit)
      }

      // reference model
      val golden = eng.feval[Array[Int]]("matintrlv", inputBlock.toArray, Array(row), Array(col))
      val yours = outputBlock

      println(golden.mkString(""))
      println(yours.mkString(""))
      printlnGreen("I/O at the first cycle")
      println(golden.take(parallelFactor).mkString(""))
      println(yours.take(parallelFactor).mkString(""))

      golden.grouped(parallelFactor).zip(outputBlock.grouped(parallelFactor)).zipWithIndex
        .foreach { case ((ints, ints1), i) => if (ints.sum != ints1.sum) println(s"${ints.sum}, ${ints1.sum}, $i") }

      assert(golden.mkString("") == yours.mkString(""))
    }
  }
}
