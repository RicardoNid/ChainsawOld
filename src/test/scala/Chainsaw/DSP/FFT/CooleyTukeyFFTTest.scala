package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.algos.MatlabRefs.{dft, idft}
import Chainsaw.algos._
import breeze.linalg.{DenseVector, max}
import breeze.numerics._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import spinal.core._
import spinal.core.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.collection.mutable.ArrayBuffer


class CooleyTukeyFFTTest() extends AnyFlatSpec with Matchers {

  /** the fully-parameterized fft/ifft testbench
   */
  def testFFTHardware(testSize: Int,
                      testLength: Int, factors: Seq[Int], period: Int = 1,
                      inverse: Boolean = false, realSequence: Boolean = false,
                      dataType: HardType[SFix], coeffType: HardType[SFix],
                      epsilon: Double = 1E-2) = {

    val normalizedData = (0 until testSize).map(_ => ChainsawRand.nextComplexDV(testLength))
    // generate testcases according to requirement
    val testCases: Seq[DenseVector[BComplex]] = {
      if (!inverse && !realSequence) normalizedData
      else if (inverse && !realSequence) normalizedData.map(Dft.dft(_))
      else if (!inverse && realSequence) normalizedData.map(vec => vec.map(complex => BComplex(complex.real, 0.0)))
      else normalizedData.map(vec => Dft.dft(vec.map(complex => BComplex(complex.real, 0.0))))
    }
    val goldens: Seq[DenseVector[BComplex]] = if (!inverse) testCases.map(Dft.dft(_)) else testCases.map(Dft.idft(_))


    SimConfig.withWave.compile {
      CooleyTukeyFFT(N = testLength, factors = factors, inverse = inverse, dataType, coeffType)
    }
      .doSim { dut =>

        dut.clockDomain.forkStimulus(2)
        dut.dataIn.clear()
        dut.clockDomain.waitSampling()

        // TODO: eliminate the inconsistency of the input and output type
        val dutResults: Seq[Seq[BComplex]] = flowPeekPoke(
          dut = dut,
          testCases = testCases.map(_.toArray.toSeq),
          dataIn = dut.dataIn,
          dataOut = dut.dataOut,
          latency = dut.latency
        )

        goldens.zip(dutResults).map { case (golden, dut) =>
          val diff = golden - dut.asDv
          println(golden)
          println(dut.asDv)
          assert(golden ~= (dut.asDv, epsilon), max(abs(diff)))
        }
      }
  }

  def testCooleyTukeyBackToBackHardware(testLength: Int, pF: Int,
                                        factors1: Seq[Int], factors2: Seq[Int],
                                        inverse: Boolean = false, epsilon: Double = 0.1) = {
    SimConfig.withWave.compile(CooleyTukeyBackToBack(
      N = testLength, pF = pF,
      factors1 = factors1, factors2 = factors2, inverse = inverse,
      dataType, coeffType)).doSim { dut =>

      val testBase = (0 until testLength).map(i => new BComplex(ChainsawRand.nextDouble() - 0.5, ChainsawRand.nextDouble() - 0.5)).toArray
      val testComplex = testBase

      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataOut.ready #= true
      clockDomain.waitSampling()

      val dutResult = ArrayBuffer[BComplex]()
      val monitor = fork {
        while (true) {
          if (dataOut.valid.toBoolean) {
            dutResult ++= dataOut.payload.map(complex => new BComplex(complex.real.toDouble, complex.imag.toDouble))
          }
          clockDomain.waitSampling()
        }
      }

      dataOut.ready #= true
      testComplex.grouped(pF).toSeq.foreach { data =>
        dataIn.valid #= true
        dataIn.payload.zip(data).foreach { case (port, complex) =>
          port.real #= complex.real
          port.imag #= complex.imag
        }
        clockDomain.waitSampling()
      }
      dataIn.valid #= false

      clockDomain.waitSampling(200)

      val golden = if (!inverse) Refs.FFT(testComplex) else Refs.IFFT(testComplex).map(_ * testLength)
      //      val golden = if (!inverse) Refs.FFT(testComplex) else Refs.IFFT(testComplex)
      println(testComplex.grouped(testLength / pF).toSeq.map(_.mkString(" ")).mkString("\n"))
      println(testComplex.reduce(_ + _).real, testComplex.reduce(_ + _).imag)

      println(dutResult.zip(golden).map { case (complex, complex1) => complex.toString + "####" + complex1.toString }.mkString("\n"))
      dutResult should not be empty
      assert(golden.zip(dutResult).forall { case (complex, complex1) => complex.sameAs(complex1, epsilon) })
    }
  }

  // the simple test we use in this file
  // you can specify length, factors, inverse, and epsilon
  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))
  val epsilon = 1E-2
  val simpleTest: (Int, Seq[Int], Boolean, Double) => Unit = testFFTHardware(10, _, _, 1, _, false, dataType, coeffType, _)

  def simpleRadixRTest(length: Int, radix: Int, inverse: Boolean, epsilon: Double): Unit = {
    require(isPowR(length, radix))
    val stages = log(radix.toDouble, length.toDouble).toInt
    val factors = Seq.fill(stages)(radix)
    printlnGreen(factors.mkString(" "))
    simpleTest(length, factors, inverse, epsilon)
  }

  it should "work for radix-2, 4, 8 fft" in {
    val testFft: Int => Unit = simpleRadixRTest(64, _, false, 0.1)
    val testIfft: Int => Unit = simpleRadixRTest(64, _, true, 0.5)
    Seq(2, 4, 8).foreach { radix =>
      testFft(radix)
      if (i != 8) testIfft(radix) // skip radix-8 inverse
      logger.info(s"radix-$radix fft/ifft passed")
    }
  }

  val backToBackTest: (Int, Seq[Int], Int, Boolean, Double) => Unit = testFFTHardware(10, _, _, _, _, false, dataType, coeffType, _)

  it should "work when folded by \"back to back\" architecture " in {
    testCooleyTukeyBackToBackHardware(16, 4, Seq(4), Seq(4), inverse = false)
    printlnGreen(s"16-point FFT as 4*4 back to back passed")
    testCooleyTukeyBackToBackHardware(16, 4, Seq(4), Seq(4), inverse = true)
    printlnGreen(s"16-point IFFT as 4*4 back to back passed")

    testCooleyTukeyBackToBackHardware(32, 8, Seq(4, 2), Seq(4), inverse = false, epsilon = 0.2)
    printlnGreen(s"32-point IFFT as 8*4 back to back passed")
    testCooleyTukeyBackToBackHardware(32, 8, Seq(4, 2), Seq(4), inverse = true, epsilon = 0.2)
    printlnGreen(s"32-point FFT as 8*4 back to back passed")

    testCooleyTukeyBackToBackHardware(64, 8, Seq(4, 2), Seq(4, 2), inverse = false, epsilon = 0.5)
    printlnGreen(s"64-point FFT as 8*8 back to back passed")
    testCooleyTukeyBackToBackHardware(64, 8, Seq(4, 2), Seq(4, 2), inverse = true, epsilon = 0.5)
    printlnGreen(s"64-point IFFT as 8*8 back to back passed")
  }

  "back-to-back Cooley-Tukey FFTs" should "pass the following tests" in {
    testCooleyTukeyBackToBackHardware(16, 4, Seq(4), Seq(4), inverse = false)
    printlnGreen(s"16-point FFT as 4*4 back to back passed")
    testCooleyTukeyBackToBackHardware(16, 4, Seq(4), Seq(4), inverse = true)
    printlnGreen(s"16-point IFFT as 4*4 back to back passed")

    testCooleyTukeyBackToBackHardware(32, 8, Seq(4, 2), Seq(4), inverse = false, epsilon = 0.2)
    printlnGreen(s"32-point IFFT as 8*4 back to back passed")
    testCooleyTukeyBackToBackHardware(32, 8, Seq(4, 2), Seq(4), inverse = true, epsilon = 0.2)
    printlnGreen(s"32-point FFT as 8*4 back to back passed")

    testCooleyTukeyBackToBackHardware(64, 8, Seq(4, 2), Seq(4, 2), inverse = false, epsilon = 0.5)
    printlnGreen(s"64-point FFT as 8*8 back to back passed")
    testCooleyTukeyBackToBackHardware(64, 8, Seq(4, 2), Seq(4, 2), inverse = true, epsilon = 0.5)
    printlnGreen(s"64-point IFFT as 8*8 back to back passed")
  }

}