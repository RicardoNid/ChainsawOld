package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.algos._
import Chainsaw.dspTest._
import breeze.linalg.{DenseVector, max}
import breeze.numerics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

class CooleyTukeyFFTTest() extends AnyFlatSpec with Matchers {

  // TODO: fix this test as the meaning of factors and shifts changed

  /** the fully-parameterized fft/ifft testbench
   */
  // TODO: should not test real valued ones in this method as it has a different interface
  def testFFTHardware(testSize: Int,
                      testLength: Int, factors: Seq[Int], parallelism: Int = 1,
                      inverse: Boolean = false, realSequence: Boolean = false,
                      dataType: HardType[SFix], coeffType: HardType[SFix],
                      epsilon: Double = 1E-2): Unit = {

    // generate factors
    require(testLength % parallelism.abs == 0 && parallelism <= 1)
    val parallelFactor = if (parallelism == 1) testLength else testLength / (-parallelism)
    val prods = (1 to factors.size).map(factors.take(_).product)
    val splitPoint = prods.indexWhere(_ == parallelFactor)

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val testCases: Seq[DenseVector[BComplex]] = {
      if (!inverse && !realSequence) normalizedData
      else if (inverse && !realSequence) normalizedData.map(Dft.dft(_))
      else if (!inverse && realSequence) normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
      else normalizedData.map(vec => Dft.dft(vec.map(complex => BComplex(complex.real, 0.0))))
    }
    val goldens: Seq[DenseVector[BComplex]] = if (!inverse) testCases.map(Dft.dft(_)) else testCases.map(Dft.idft(_))

    SimConfig.withFstWave.compile {
      if (parallelism == 1) CooleyTukeyFFT(N = testLength, inverse = inverse, dataType, coeffType, factors = factors)
      else AdaptiveCooleyTukeyFFT(testLength, parallelFactor, inverse, dataType, coeffType, factors)
    }.doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      dut.dataIn.clear()
      dut.clockDomain.waitSampling()

      val groupedTestCases = testCases.map(_.toArray.toSeq.grouped(parallelFactor).toSeq).flatten
      val groupedGoldens = goldens.map(_.toArray.toSeq.grouped(parallelFactor).toSeq).flatten

      val dutResults: Seq[Seq[BComplex]] = flowPeekPoke(
        dut = dut,
        testCases = groupedTestCases,
        dataIn = dut.dataIn,
        dataOut = dut.dataOut,
        latency = dut.latency
      )._2

      groupedGoldens.zip(dutResults).map { case (golden, dut) =>
        val diff = golden.toDv - dut.toDv
        println(golden)
        println(dut.toDv)
        //        println(golden.zip(dut).map{ case (complex, complex1) => complex - complex1}
        //          .filter(_.abs > 0.01).mkString(" "))
        assert(golden.toDv ~= (dut.toDv, epsilon), max(abs(diff)))
      }
    }
  }

  // the simple test we use in this file
  // you can specify length, factors, inverse, and epsilon
  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val epsilon = 1E-2
  val simpleTest: (Int, Seq[Int], Int, Boolean, Double) => Unit = testFFTHardware(10, _, _, _, _, false, dataType, coeffType, _)

  def simpleRadixRTest(length: Int, radix: Int, parallelism: Int, inverse: Boolean, epsilon: Double): Unit = {
    require(isPowR(length, radix))
    val stages = log(radix.toDouble, length.toDouble).toInt
    val factors = Seq.fill(stages)(radix)
    simpleTest(length, factors, parallelism, inverse, epsilon)
  }

  it should "work for different radixes" in {
    val testFft: Int => Unit = simpleRadixRTest(64, _, 1, inverse = false, 0.1)
    val testIfft: Int => Unit = simpleRadixRTest(64, _, 1, inverse = true, 0.5)
    Seq(2, 4, 8).foreach { radix =>
      testFft(radix)
      testIfft(radix) // skip radix-8 inverse
      logger.info(s"radix-$radix fft/ifft passed")
    }
  }

  it should "work for different parallelism" in {
    val testFft: Int => Unit = simpleRadixRTest(64, 2, _, inverse = false, 0.1)
    val testIfft: Int => Unit = simpleRadixRTest(64, 2, _, inverse = true, 0.5)
    Seq(-2, -4, -8).foreach { parallelism =>
      testFft(parallelism)
      testIfft(parallelism)
    }
  }

  it should "synth for 128-point" in {
    val dataType = HardType(SFix(7 exp, 16 bits))
    val coeffType = HardType(SFix(1 exp, 16 bits))
    //    VivadoSynthForTiming(CooleyTukeyFFT(128, false, dataType, coeffType, Seq(4, 4, 4, 2)))
    VivadoSynthForTiming(CooleyTukeyFFT(128, false, dataType, coeffType, Seq(8, 8, 2)))
  }

  it should "synth for FTN" in {
    val dspType = HardType(SFix(7 exp, 16 bits))
    val coeffType = HardType(SFix(1 exp, 16 bits))
    VivadoSynthForTiming(AdaptiveCooleyTukeyFFT(512, 256, false, dspType, coeffType, Seq(4, 4, 4, 4, 2)), name = "RxIfft")
    VivadoSynthForTiming(AdaptiveCooleyTukeyFFT(512, 256, false, dspType, coeffType, Seq(8, 8, 4, 2)), name = "RxIfftR8")
    VivadoSynthForTiming(AdaptiveCooleyTukeyFFT(512, 256, true, dspType, coeffType, Seq(4, 4, 4, 4, 2)), name = "RxFft")
    VivadoSynthForTiming(AdaptiveCooleyTukeyFFT(512, 256, true, dspType, coeffType, Seq(8, 8, 4, 2)), name = "RxFftR8")
  }

  it should "work with P2S2P" in {
    val testCases = (0 until 10).map(_ => ChainsawRand.nextComplexDV(64))
    val goldens: Seq[DenseVector[BComplex]] = testCases.map(Dft.dft(_))

    SimConfig.withFstWave.compile {
      new Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

        import DSP.{P2S, S2P}

        val complexType = HardType(ComplexNumber(dataType))
        val p2s = P2S(64, 32, complexType)
        val s2p = S2P(32, 64, complexType)
        val core = AdaptiveCooleyTukeyFFT(64, 32, inverse = false, dataType, coeffType, Seq(4, 4, 2), Seq(2))
        override val dataIn = slave Stream Vec(complexType(), 64)
        override val dataOut = master Stream Vec(complexType(), 64)
        override val latency = core.latency

        dataIn >> p2s.dataIn
        p2s.dataOut >> core.dataIn
        core.dataOut >> s2p.dataIn
        s2p.dataOut >> dataOut
      }

    }.doSim { dut =>

      dut.clockDomain.forkStimulus(2)
      dut.dataIn.clear()
      dut.clockDomain.waitSampling()

      val dutResults: Seq[Seq[BComplex]] = flowPeekPoke(
        dut = dut,
        testCases = testCases.map(_.toArray.toSeq),
        dataIn = dut.dataIn,
        dataOut = dut.dataOut,
        latency = dut.latency
      )._2

      goldens.zip(dutResults).map { case (golden, dut) =>
        val diff = golden - dut.toDv
        println(golden)
        println(dut.toDv)
        assert(golden ~= (dut.toDv, epsilon), max(abs(diff)))
      }
    }
  }

  it should "synth" in VivadoSynthForTiming(
    AdaptiveCooleyTukeyFFT(32, 8, false, HardType(SFix(2 exp, -11 exp)), HardType(SFix(2 exp, -11 exp)), Seq(4, 2), Seq(4)))

  it should "synth for pipelined" in VivadoSynthForTiming(
    CooleyTukeyFFT(8, false, HardType(SFix(2 exp, -11 exp)), HardType(SFix(2 exp, -11 exp)), Seq(4, 2)))

  it should "impl for pipelined" in VivadoImplForTiming(
    CooleyTukeyFFT(N = 16, inverse = false, HardType(SFix(2 exp, -11 exp)), HardType(SFix(2 exp, -11 exp)), Seq(4, 4)))

  it should "be better than Xilinx IP" in {
    val fp18 = HardType(SFix(1 exp, -16 exp))
    val fp24 = HardType(SFix(6 exp, -17 exp))
    VivadoSynth(
      AdaptiveCooleyTukeyFFT(1024, 128, false, fp24, fp18, Seq(4, 4, 4, 2, 8), Seq(0, 0, 0, 0, 0, 0))
    )
  }
}