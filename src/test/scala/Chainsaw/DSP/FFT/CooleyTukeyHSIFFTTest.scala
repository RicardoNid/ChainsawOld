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

  def testHSIFFTHardware(testSize: Int, testLength: Int,
                         pF: Int, factors: Seq[Int],
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
      name = "testHSIFFT", dut = CooleyTukeyHSIFFT(testLength, pF, dataType, coeffType, factors),
      testCases = testCases.map(_.toArray.toSeq.grouped(pF)).flatten,
      golden = goldens.map(_.toArray.toSeq.map(_.real).grouped(pF)).flatten,
      testMetric = TestMetric.APPROXIMATE, epsilon = 1E-2
    )
  }

  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val simpleTest: (Int, Int, Seq[Int]) => Unit = testHSIFFTHardware(10, _, _, _, dataType, coeffType)

  it should "work" in {
    Seq(8, 16, 32, 64).zip(Seq(3, 4, 5, 6)).foreach { case (n, stage) => simpleTest(n, n, Seq.fill(stage)(2)) }
  }

  it should "work for folded situation" in {
    Seq(32, 64).zip(Seq(5, 6))
      .foreach { case (n, stage) => simpleTest(n, n / 2, Seq.fill(stage)(2)) }
    simpleTest(64, 16, Seq(4, 2, 4, 2))
  }
}
