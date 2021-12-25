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
                         testLength: Int, factors: Seq[Int],
                         dataType: HardType[SFix], coeffType: HardType[SFix],
                         epsilon: Double = 1E-2): Unit = {

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val reals: Seq[DenseVector[BComplex]] = normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
    val testCases: Seq[DenseVector[BComplex]] = reals.map(Dft.dft(_))
    val goldens = testCases.map(Dft.idft(_)) // in fact, goldens = reals *:* N

    SimConfig.withWave.compile {
      CooleyTukeyHSIFFT(testLength, factors, dataType, coeffType)
    }.doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      dut.dataIn.clear()
      dut.clockDomain.waitSampling()

      val dutResults: Seq[Seq[Double]] = flowPeekPoke(
        dut = dut,
        testCases = testCases.map(_.toArray.toSeq),
        dataIn = dut.dataIn,
        dataOut = dut.dataOut,
        latency = dut.latency
      )

      println(s"input0 ${testCases(0)}")
      println(s"input1 ${testCases(1)}")
      val (golden0, golden1) = Dft.rvidftByDouble(testCases(0), testCases(1))
      println(s"golden0 \n$golden0\n${goldens(0)}")
      println(s"golden1 \n$golden1\n${goldens(1)}")

      assert(dutResults.nonEmpty)
      goldens.zip(dutResults).map { case (golden, dut) =>
        val dutAsComponent = dut.map(BComplex(_, 0.0)).toDv
        val diff = golden - dutAsComponent
        println(s"yours $dutAsComponent")
        println(s"golden $golden")
        assert(golden ~= (dutAsComponent, epsilon), max(abs(diff)))
      }

    }
  }

  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val simpleTest: (Int, Seq[Int]) => Unit = testHSIFFTHardware(10, _, _, dataType, coeffType, 1E-2)

  it should "work" in {
    Seq(8, 16, 32, 64).zip(Seq(3, 4, 5, 6)).foreach { case (n, stage) => simpleTest(n, Seq.fill(stage)(2)) }
  }

  it should "synth for FTN" in {
    val dataType = HardType(SFix(7 exp, 16 bits))
    val coeffType = HardType(SFix(1 exp, 16 bits))
    VivadoSynth(CooleyTukeyHSIFFT(512, Seq(4, 4, 4, 4, 2), dataType, coeffType))
  }

}
