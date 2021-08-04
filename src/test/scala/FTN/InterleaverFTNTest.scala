package FTN


import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class InterleaverFTNTest extends AnyFunSuite {
  test("testInterleaver for FTN") {
    SimConfig.withWave.compile(new InterleaverFTN(32, 128, 256)).doSim { dut =>
      import dut._

      val eng = AsyncEng.get()
      clockDomain.forkStimulus(2)

      running #= false
      we #= false
      clockDomain.waitSampling()

      val latency = row * col / parallelFactor
      val inputs = (0 until latency).map { _ =>
        val bytes = Array.fill(parallelFactor / 8)(1.toByte)
        DSPRand.nextBytes(bytes)
        BigInt(bytes).abs
      }
      val inputFlatten = inputs.map(_.toString(2).padToLeft(parallelFactor, '0').map(_.asDigit)).flatten
      val outputFlatten = ArrayBuffer[Int]()

      (0 until latency).foreach { i =>
        running #= true
        we #= true
        dataIn #= inputs(i)
        clockDomain.waitSampling()
      }

      (0 until latency).foreach { i =>
        running #= true
        we #= false
        clockDomain.waitSampling()
        outputFlatten ++= dataOut.toBigInt.toString(2).padToLeft(parallelFactor, '0').map(_.asDigit)
      }

      // reference model
      val golden = eng.feval[Array[Int]]("matintrlv", inputFlatten.toArray, Array(row), Array(col))
      val yours = outputFlatten

      println(golden.mkString(""))
      println(yours.mkString(""))
      printlnGreen("first cycle of I/O ")
      println(golden.take(parallelFactor).mkString(""))
      println(yours.take(parallelFactor).mkString(""))

      golden.grouped(parallelFactor).zip(outputFlatten.grouped(parallelFactor)).zipWithIndex
        .foreach { case ((ints, ints1), i) => if (ints.sum != ints1.sum) println(s"${ints.sum}, ${ints1.sum}, $i") }

      assert(golden.mkString("") == yours.mkString(""))
    }
  }
}
