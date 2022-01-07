package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.algos.{Dft, dvComplexUtil}
import Chainsaw.dspTest._
import breeze.linalg._
import breeze.numerics._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.core.sim._

class CooleyTukeyHSIFFTTest extends AnyFlatSpec {

  behavior of "CooleyTukeyHSIFFTTest"

  def testHSIFFTHardware(testSize: Int,
                         testLength: Int, factors1: Seq[Int], factors2: Seq[Int],
                         dataType: HardType[SFix], coeffType: HardType[SFix]): Unit = {

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val reals: Seq[DenseVector[BComplex]] = normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
    val testCases: Seq[DenseVector[BComplex]] = reals.map { real =>
      val ret = Dft.dft(real)
      ret(0) = BComplex(0.0, 0.0) // fot FTN
      ret(testLength / 2) = BComplex(0.0, 0.0)
      ret
    }
    val goldens: Seq[DenseVector[BComplex]] = testCases.map(Dft.idft(_)) // in fact, goldens = reals *:* N

    doFlowPeekPokeTest(
      name = "testHSIFFT", dut = CooleyTukeyHSIFFT(testLength, factors1, factors2, dataType, coeffType),
      testCases = testCases.map(_.toArray.toSeq.grouped(factors1.product * 2)).flatten,
      golden = goldens.map(_.toArray.toSeq.map(_.real).grouped(factors1.product * 2)).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2
    )
  }

  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val simpleTest: (Int, Seq[Int], Seq[Int]) => Unit = testHSIFFTHardware(10, _, _, _, dataType, coeffType)

  it should "work" in {
    Seq(8, 16, 32, 64).zip(Seq(3, 4, 5, 6)).foreach { case (n, stage) => simpleTest(n, Seq.fill(stage - 1)(2), Seq(2)) }
  }

  it should "work for folded situation" in {
    Seq(32, 64).zip(Seq(5, 6))
      .foreach { case (n, stage) => simpleTest(n, Seq.fill(stage - 2)(2), Seq(4)) }
    Seq(64).zip(Seq(6))
      .foreach { case (n, stage) => simpleTest(n, Seq.fill(stage - 3)(2), Seq(4, 2)) }
  }

  it should "synth for FTN" in {
    val dataType = HardType(SFix(7 exp, 16 bits))
    val coeffType = HardType(SFix(1 exp, 16 bits))
    // for Rx
    VivadoSynth(CooleyTukeyHSIFFT(512, Seq(4, 4, 4, 4), Seq(2), dataType, coeffType))
    // for Tx
  }

}
