package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.algos._
import Chainsaw.dspTest._
import breeze.linalg.{DenseVector, max}
import breeze.numerics._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

class CooleyTukeyRVFFTTest extends AnyFlatSpec {

  behavior of "CooleyTukeyRVFFTTest"

  def testRVFFTHardware(testSize: Int,
                        testLength: Int, factors1: Seq[Int], factors2: Seq[Int],
                        dataType: HardType[SFix], coeffType: HardType[SFix]): Unit = {

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val testCases: Seq[DenseVector[BComplex]] = normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
    val goldens: Seq[DenseVector[BComplex]] = testCases.map(Dft.dft(_))

    doFlowPeekPokeTest(
      name = "testRVFFT", dut = CooleyTukeyRVFFT(testLength, factors1, factors2, dataType, coeffType),
      testCases = testCases.map(_.map(_.real).toArray.toSeq.grouped(factors1.product * 2).toSeq).flatten,
      golden = goldens.map(_.toArray.toSeq.grouped(factors1.product * 2).toSeq).flatten,
      initLength = 0,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1 * log2(testSize)
    )
  }

  val dataType = HardType(SFix(7 exp, -8 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val simpleTest: (Int, Seq[Int], Seq[Int]) => Unit = testRVFFTHardware(10, _, _, _, dataType, coeffType)

  it should "work" in {
    Seq(8, 16, 32, 64).zip(Seq(3, 4, 5, 6))
      .foreach { case (n, stage) => simpleTest(n, Seq.fill(stage - 1)(2), Seq(2)) }
    Seq(8, 32).zip(Seq(1, 2))
      .foreach { case (n, stage) => simpleTest(n, Seq.fill(stage)(4), Seq(2)) }
  }

  it should "work for folded situation" in {
    Seq(32, 64).zip(Seq(5, 6))
      .foreach { case (n, stage) => simpleTest(n, Seq.fill(stage - 2)(2), Seq(4)) }
    Seq(64).zip(Seq(6))
      .foreach { case (n, stage) => simpleTest(n, Seq.fill(stage - 3)(2), Seq(4, 2)) }
  }

  it should "synth for FTN" in {
    // for Rx
    VivadoSynth(CooleyTukeyRVFFT(512, Seq(4, 4, 4, 4), Seq(2), dataType, coeffType))
  }

}
