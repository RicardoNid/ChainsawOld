package Chainsaw.comm.qam

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.math.sqrt

import scala.collection.mutable.ArrayBuffer

class QAMDeModTest extends AnyFlatSpec {

  def runSim() = {

    val bitAlloc = (0 until 10).map(_ => (ChainsawRand.nextInt(3) + 1) * 2)
    printlnGreen(bitAlloc.mkString(" "))
    val powAlloc = (0 until 10).map(_ => ChainsawRand.nextDouble())
    //    val powAlloc = Seq.fill(10)(1.0)
    val testUpperbound = 1

    val testCase: Seq[BComplex] = powAlloc.map(ChainsawRand.nextComplex(-testUpperbound, testUpperbound) * _)
    var dutResults = ArrayBuffer[BigInt]()

    SimConfig.withWave.compile(QamdemodWithAlloc(bitAlloc, powAlloc, HardType(ComplexNumber(3, -14)))).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dutResults = flowPeekPoke(dut, Seq(testCase), dataIn, dataOut, 1)._2
    }

    val golden: Array[Int] = testCase.zip(powAlloc.zip(bitAlloc)).map { case (complex, (pow, bit)) => Refs.qamdemod(complex / sqrt(pow) * Refs.getQAMRms(bit), bit, true) }.toArray

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

  runSim()

}
