package Chainsaw.DSP.interleave

import org.scalatest.funsuite.AnyFunSuite

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import scala.collection.mutable.ArrayBuffer

class MatInterleaverTest extends AnyFunSuite {
  test("testInterleaver for FTN") {
    SimConfig.withWave.compile(new MatInterleaver(32, 128, 256, 256, () => UInt(8 bits))).doSim { dut =>
      import dut._

      val eng = AsyncEng.get()
      clockDomain.forkStimulus(2)

      running #= false
      we #= false
      clockDomain.waitSampling()

      val latency = row * col / pFIn
      val inputs = (0 until latency).map { _ =>
        val bytes = Array.fill(pFIn / 8)(1.toByte)
        DSPRand.nextBytes(bytes)
        BigInt(bytes).abs
      }
      val inputFlatten = inputs.map(_.toString(2).padToLeft(pFIn, '0').map(_.asDigit)).flatten
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
        outputFlatten ++= dataOut.toBigInt.toString(2).padToLeft(pFIn, '0').map(_.asDigit)
      }
      val yours = outputFlatten

      // reference model
      val golden = eng.feval[Array[Int]]("matintrlv", inputFlatten.toArray, Array(row), Array(col))


      println("golden: " + toWordsHexString(BigInt(golden.mkString(""), 2), pFIn, latency * pFIn / pFIn))
      println("yours:  " + toWordsHexString(BigInt(yours.mkString(""), 2), pFIn, latency * pFIn / pFIn))
      printlnGreen("first cycle of I/O ")
      println("golden: " + toWordsHexString(BigInt(golden.take(pFIn).mkString(""), 2), pFIn, 1))
      println("yours:  " + toWordsHexString(BigInt(yours.take(pFIn).mkString(""), 2), pFIn, 1))

      golden.grouped(pFIn).zip(outputFlatten.grouped(pFIn)).zipWithIndex
        .foreach { case ((ints, ints1), i) => if (ints.sum != ints1.sum) println(s"${ints.sum}, ${ints1.sum}, $i") }

      assert(golden.mkString("") == yours.mkString(""))
    }
  }
}

