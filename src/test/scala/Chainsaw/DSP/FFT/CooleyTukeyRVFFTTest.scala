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
                        testLength: Int, factors: Seq[Int],
                        dataType: HardType[SFix], coeffType: HardType[SFix],
                        epsilon: Double = 1E-2): Unit = {

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val testCases: Seq[DenseVector[BComplex]] = normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
    val goldens: Seq[DenseVector[BComplex]] = testCases.map(Dft.dft(_))

    SimConfig.withWave.compile {
      CooleyTukeyRVFFT(testLength, factors, dataType, coeffType)
    }.doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      dut.dataIn.clear()
      dut.clockDomain.waitSampling()

      val dutResults: Seq[Seq[BComplex]] = flowPeekPoke(
        dut = dut,
        testCases = testCases.map(_.map(_.real).toArray.toSeq),
        dataIn = dut.dataIn,
        dataOut = dut.dataOut,
        latency = dut.latency
      )

      assert(dutResults.nonEmpty)
      goldens.zip(dutResults).map { case (golden, dut) =>
        val diff = golden - dut.toDv
        assert(golden ~= (dut.toDv, epsilon), max(abs(diff)))
      }

    }
  }

  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val simpleTest: (Int, Seq[Int]) => Unit = testRVFFTHardware(10, _, _, dataType, coeffType, 1E-2)

  it should "work" in {
    Seq(8, 16, 32, 64).zip(Seq(3, 4, 5, 6)).foreach { case (n, stage) => simpleTest(n, Seq.fill(stage)(2)) }
  }

}
