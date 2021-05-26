package FTN

import Chainsaw._
import com.mathworks.matlab.types.Complex
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class DynamicQammodDUT(bitsAllocated: Array[Int]) extends DSPDUTTiming[Bits, Vec[Real]] {
  override val input: Bits = in Bits (bitsAllocated.sum bits)
  val dynamicQammod = DynamicQammod(input, bitsAllocated)
  override val output: Vec[Real] = out(dynamicQammod.implicitValue)
  override val timing: TimingInfo = TimingInfo(1, 1, 1, 1)
}

class DynamicQammodDUTForSynth(bitsAllocated: Array[Int]) extends DSPDUTTiming[Bits, Vec[Real]] {
  override val input: Bits = in Bits (bitsAllocated.sum bits)
  val dynamicQammod = DynamicQammod(RegNext(input), bitsAllocated)
  override val output: Vec[Real] = out(dynamicQammod.implicitValue)
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)
}

class DynamicQammodSim(bitsAllocated: Array[Int]) extends DynamicQammodDUT(bitsAllocated) with DSPSimTiming[Bits, Vec[Real], Array[Int], Array[Complex]] {
  require(bitsAllocated.sum % 8 == 0, "as you need to poke them as bytes")
  override def poke(testCase: Array[Int], input: Bits): Unit = {
    val inputBits = testCase.zip(bitsAllocated)
      .map { case (number, bitAllocated) => number.toBinaryString.reverse.padTo(bitAllocated, '0').reverse }
      .flatten
    val inputBytes = inputBits
      .grouped(8).toArray
      .map(_.reverse.zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum.toByte)
    printlnWhenDebug(s"poke to dut    ${inputBits.mkString(" ")}")
    printlnWhenDebug(s"poke to dut bytes ${inputBytes.mkString(" ")}")
    input #= inputBytes.reverse // cancel the reverse behavior of #= by an extra reverse
  }
  override def peek(output: Vec[Real]): Array[Complex] = {
    output.grouped(2).toArray.map(pair => new Complex(pair(0).toDouble, pair(1).toDouble))
  }
  override def referenceModel(testCase: Array[Int]): Array[Complex] = dynamicQammod.referenceModel(testCase)
  override def isValid(refResult: Array[Complex], dutResult: Array[Complex]): Boolean =
    refResult.zip(dutResult).forall { pair =>
      pair._1.real == pair._2.real
      pair._1.imag == pair._2.imag
    }
  override def messageWhenInvalid(testCase: Array[Int], refResult: Array[Complex], dutResult: Array[Complex]): String = {
    s"testCase: ${testCase.map(_.toString.padTo(15, ' ')).mkString("")}\n" +
      s"bitAlloc: ${bitsAllocated.map(_.toString.padTo(15, ' ')).mkString("")}\n" +
      s"golden:   ${refResult.map(_.toString.padTo(15, ' ')).mkString("")}\n" +
      s"yours:    ${dutResult.map(_.toString.padTo(15, ' ')).mkString("")}\n"
  }
  override def messageWhenValid(testCase: Array[Int], refResult: Array[Complex], dutResult: Array[Complex]): String =
    messageWhenInvalid(testCase, refResult, dutResult)
}

class DynamicQammodTest extends AnyFunSuite {
  test("testDynamicQammod") {
    ChainsawDebug = true

    def doTest(bitsAllocated: Array[Int]): Unit = {
      SimConfig.withWave.compile(new DynamicQammodSim(bitsAllocated)).doSim { dut =>
        dut.sim()
        (0 until 1).foreach(_ =>
          dut.insertTestCase(bitsAllocated.map(bitAllocated => DSPRand.nextInt(1 << bitAllocated))))
        dut.simDone()
      }
    }
    doTest(Array(1, 2, 3, 4, 6))
  }

  test("synthDynamicQammod"){
    VivadoSynth(new DynamicQammodDUTForSynth(Array.fill(224)(4)))
  }
}
