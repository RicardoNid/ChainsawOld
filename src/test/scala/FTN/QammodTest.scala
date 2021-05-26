package FTN

import Chainsaw._
import com.mathworks.matlab.types.Complex
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

class QammodDUT(symbolCount: Int, bitsAllocated: Int) extends DSPDUTTiming[Bits, Vec[Real]] {
  override val input: Bits = in Bits (symbolCount * bitsAllocated bits)
  val qammod = new Qammod(input, bitsAllocated)
  override val output: Vec[Real] = out(qammod.implicitValue)
  override val timing: TimingInfo = qammod.timing
}

class QammodSim(symbolCount: Int, bitsAllocated: Int) extends QammodDUT(symbolCount, bitsAllocated) with DSPSimTiming[Bits, Vec[Real], Array[Int], Array[Complex]] {
  require((symbolCount * bitsAllocated) % 8 == 0, "as you need to poke them as bytes")
  override def poke(testCase: Array[Int], input: Bits): Unit = {
    val inputBits = testCase
      .map(_.toBinaryString.reverse.padTo(bitsAllocated, '0').reverse)
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
  override def referenceModel(testCase: Array[Int]): Array[Complex] = qammod.referenceModel(testCase)
  override def isValid(refResult: Array[Complex], dutResult: Array[Complex]): Boolean =
    refResult.zip(dutResult).forall { pair =>
      pair._1.real == pair._2.real
      pair._1.imag == pair._2.imag
    }
  override def messageWhenInvalid(testCase: Array[Int], refResult: Array[Complex], dutResult: Array[Complex]): String =
    s"testCase: ${testCase.map(_.toString.padTo(15, ' ')).mkString("")}\n" +
      s"golden:   ${refResult.map(_.toString.padTo(15, ' ')).mkString("")}\n" +
      s"yours:    ${dutResult.map(_.toString.padTo(15, ' ')).mkString("")}\n"
  override def messageWhenValid(testCase: Array[Int], refResult: Array[Complex], dutResult: Array[Complex]): String =
    messageWhenInvalid(testCase, refResult, dutResult)
}

class QammodTest extends AnyFunSuite {
  test("testQammod") {
    //    ChainsawDebug = true
    def doTest(symbolCount:Int, bitsAllocated:Int): Unit ={
      SimConfig.withWave.compile(new QammodSim(symbolCount, bitsAllocated)).doSim { dut =>
        dut.sim()
        (0 until 5).foreach(_ =>
          dut.insertTestCase((0 until symbolCount).map(_ => DSPRand.nextInt(1 << bitsAllocated)).toArray))
        dut.simDone()
      }
    }

    doTest(224, 4)
    doTest(128, 3)
  }
}
