package Chainsaw.Communication.qam

import Chainsaw._
import Chainsaw.dspTest._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer
import scala.math.ceil

class QAMDeModCoreTest extends AnyFlatSpec {

  def runSim(bitsAllocated: Int) = {

    printlnGreen(s"testing QAM${1 << bitsAllocated}")

    def isInt = (value: Double) => value == ceil(value)

    val testUpperbound = Refs.getQAMValues(bitsAllocated).map(_.modulus).max
    val testCasesBeforeNorm = (0 until 200).map(_ => DSPRand.nextComplex(-testUpperbound, testUpperbound))
      .filterNot(complex => isInt(complex.real) || isInt((complex.imag)))
    val testCases = testCasesBeforeNorm.map(_ / Refs.getQAMRms(bitsAllocated))
    var dutResults = ArrayBuffer[BigInt]()

    SimConfig.withWave.compile(QAMDeModCore(HardType(ComplexNumber(1, -14)), bitsAllocated)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dutResults = flowPeekPokeRound(dut, testCases, dataIn, dataOut, 1)
    }

    val golden: Array[Int] = Refs.qamdemod(testCasesBeforeNorm.toArray, bitsAllocated)

    println(s"yours  ${dutResults.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    println(s"golden ${golden.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    val diff = golden.zip(dutResults).map { case (g, y) => (g - y).abs }
    if (!diff.forall(_ == 0) ){
      println(s"diff   ${diff.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
      println(s"diff types ${diff.map(_.toString.padToLeft(3, ' ')).distinct.mkString(" ")}")
      println(s"diff count ${diff.filter(_ != 0).size}")
    }
    assert(dutResults.mkString("") == golden.mkString(""))

  }

  "qamdemod core" should "have correct output" in {
    Seq(1, 2, 3, 4, 6, 8).foreach(runSim)
  }

}
