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
    val (factors1, factors2) = factors.splitAt(splitPoint + 1)

    // generate testcases according to requirement
    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    val testCases: Seq[DenseVector[BComplex]] = {
      if (!inverse && !realSequence) normalizedData
      else if (inverse && !realSequence) normalizedData.map(Dft.dft(_))
      else if (!inverse && realSequence) normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
      else normalizedData.map(vec => Dft.dft(vec.map(complex => BComplex(complex.real, 0.0))))
    }
    val goldens: Seq[DenseVector[BComplex]] = if (!inverse) testCases.map(Dft.dft(_)) else testCases.map(Dft.idft(_))

    SimConfig.withWave.compile {
      if (parallelism == 1) CooleyTukeyFFT(N = testLength, factors = factors, inverse = inverse, dataType, coeffType)
      else CooleyTukeyBackToBack(testLength, parallelFactor, factors1, factors2, inverse, dataType, coeffType)
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
      if (radix != 8) testIfft(radix) // skip radix-8 inverse
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

  it should "synth for FTN" in {
    VivadoSynth(
      new Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {
        // TODO: when using 18 bits, way too big signal would appear
        val dspType = HardType(SFix(7 exp, 16 bits))
        val core = CooleyTukeyBackToBack(512, 256, Seq(4, 4, 4, 4), Seq(2), true, dspType, dspType)
        override val dataIn = slave(cloneOf(core.dataIn))
        override val dataOut = master(cloneOf(core.dataOut))
        override val latency = core.latency + 2
        dataIn.m2sPipe() >> core.dataIn
        core.dataOut.m2sPipe() >> dataOut
      }
    )
  }

  it should "work with P2S2P" in {
    val testCases = (0 until 10).map(_ => ChainsawRand.nextComplexDV(64))
    val goldens: Seq[DenseVector[BComplex]] = testCases.map(Dft.dft(_))

    SimConfig.withWave.compile {
      new Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

        import DSP.{P2S, S2P}

        val complexType = HardType(ComplexNumber(dataType))
        val p2s = P2S(64, 32, complexType)
        val s2p = S2P(32, 64, complexType)
        val core = CooleyTukeyBackToBack(64, 32, Seq(4, 4, 2), Seq(2), inverse = false, dataType, coeffType)
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


}