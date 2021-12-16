package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.algos.Qam
import Chainsaw.dspTest._
import breeze.linalg.DenseVector
import spinal.core._
import spinal.core.sim._

import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer

class QAMDemodTest extends AnyFlatSpec {

  /** verify the function of qamdemod module
   * @param bitsAllocated number of bits per QAM symbol
   * @param powerFactor power loading factor
   * @param testSize number of test points
   */
  def testQamdemod(symbolType: HardType[ComplexNumber],
                   bitsAllocated: Int,
                   powerFactor: Double,
                   testSize:Int = 1000): Unit = {

    val modulationOrder = 1 << bitsAllocated
    logger.info(s"testing QAM$modulationOrder")

    // preparing data
    val testBound = Qam.getSymbols(modulationOrder).map(_.real).toArray.max / Qam.getRms(modulationOrder)
    val testCases = ChainsawRand.nextComplexDV(testSize).toArray.map(value => value * testBound * 2 - testBound)
    val golden = algos.Qam.qamdemod(new DenseVector(testCases), modulationOrder).toArray
    var dutResults = ArrayBuffer[BigInt]()

    // do sim
    SimConfig.withWave.compile(QAMDemod(symbolType, bitsAllocated, powerFactor)).doSim { dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dutResults = flowPeekPoke(dut, testCases.map(_ * powerFactor), dataIn, dataOut, 1)
    }

    // analyse result
    println(s"yours  ${dutResults.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    println(s"golden ${golden.map(_.toString.padToLeft(3, ' ')).mkString(" ")}")
    val diff = golden.zip(dutResults).map { case (g, y) => (g - y).abs }.filterNot(_ == 0)
    if (diff.nonEmpty) {
      logger.warn(s"diff   ${diff.map(_.toString.padToLeft(3, ' ')).mkString(" ")}" +
        s"diff types ${diff.map(_.toString.padToLeft(3, ' ')).distinct.mkString(" ")}" +
        s"diff count ${diff.size}")
    }
    assert(diff.forall(isPow2(_))) // assure that even an error(from rounding error) occurs, it is 1-bit
    logger.warn(s"1-bit error: ${diff.length} / $testSize")
  }

  "qamdemod core" should "have correct output" in {
    val symbolType = HardType(ComplexNumber(2, -9))
    val factors = (0 until 8).map(_ => ChainsawRand.nextDouble() + 0.3)
    Seq(1, 2, 3, 4, 6, 8).zip(factors).foreach { case (bitAllocated, factor) =>
      testQamdemod(symbolType, bitAllocated, factor) }
  }

}
