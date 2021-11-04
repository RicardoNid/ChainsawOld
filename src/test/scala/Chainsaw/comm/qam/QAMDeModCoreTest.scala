package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class QAMDeModCoreTest extends AnyFlatSpec {

  def runSim(bitsAllocated: Int, factor: Double = 1.0) = {

    printlnGreen(s"testing QAM${1 << bitsAllocated}")

    def isInt = (value: Double) => value == ceil(value)

    val testUpperbound = Refs.getQAMValues(bitsAllocated).map(_.modulus).max
    val testCasesBeforeNorm = (0 until 1000).map(_ => DSPRand.nextComplex(-testUpperbound, testUpperbound))
      .filterNot(complex => isInt(complex.real) || isInt((complex.imag)))
    val testCases = testCasesBeforeNorm.map(_ / Refs.getQAMRms(bitsAllocated) * factor)
    var dutResults = ArrayBuffer[BigInt]()

    SimConfig.withWave.compile(QAMDeModCore(HardType(ComplexNumber(1, -14)), bitsAllocated, factor)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dutResults = flowPeekPoke(dut, testCases, dataIn, dataOut, 1)
    }

    val golden: Array[Int] = Refs.qamdemod(testCasesBeforeNorm.toArray, bitsAllocated)

    println(s"yours  ${dutResults.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    println(s"golden ${golden.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    val diff = golden.zip(dutResults).map { case (g, y) => (g - y).abs }
    if (!diff.forall(_ == 0)) {
      printlnRed(s"diff   ${diff.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
      printlnRed(s"diff types ${diff.map(_.toString.padToLeft(3, ' ')).distinct.mkString(" ")}")
      printlnRed(s"diff count ${diff.filter(_ != 0).size}")
    }

    assert(diff.filter(_ != 0).size < 5)

  }

  "qamdemod core" should "have correct output" in {
    val factors = (0 until 8).map(_ => DSPRand.nextDouble())
    Seq(1, 2, 3, 4, 5, 6, 7, 8).zip(factors).foreach{ case (bitAllocated, factor) => runSim(bitAllocated, factor)}
  }

}
