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

  def testRVFFTHardware(testSize: Int, pF:Int,
                        testLength: Int, factors: Seq[Int],
                        dataType: HardType[SFix], coeffType: HardType[SFix]): Unit = {

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val testCases: Seq[DenseVector[BComplex]] = normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
    val goldens: Seq[DenseVector[BComplex]] = testCases.map(Dft.dft(_))

    doFlowPeekPokeTest(
      name = "testRVFFT", dut = CooleyTukeyRVFFT(testLength, pF, dataType, coeffType, factors),
      testCases = testCases.map(_.map(_.real).toArray.toSeq.grouped(pF).toSeq).flatten,
      golden = goldens.map(_.toArray.toSeq.grouped(pF).toSeq).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-1 * log2(testSize)
    )
  }

  val dataType = HardType(SFix(7 exp, -8 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val simpleTest: (Int, Int, Seq[Int]) => Unit = testRVFFTHardware(10, _, _, _, dataType, coeffType)

  it should "work" in {
    Seq(8, 16, 32, 64).zip(Seq(3, 4, 5, 6)).foreach { case (n, stage) => simpleTest(n, n, Seq.fill(stage)(2)) }
  }

  it should "work for folded situation" in {
    Seq(32, 64).zip(Seq(5, 6))
      .foreach { case (n, stage) => simpleTest(n, n / 2, Seq.fill(stage)(2)) }
    simpleTest(64, 16, Seq(4, 2, 4, 2))
  }

}
