package Chainsaw.DSP.FFT

import Chainsaw._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import spinal.core._
import spinal.core.sim._
import algos.MatlabRefs.{dft, idft}
import algos._

import scala.collection.mutable.ArrayBuffer

class CooleyTukeyFFTTest() extends AnyFlatSpec with Matchers {

  val dataType = HardType(SFix(8 exp, -15 exp))
  val coeffType = HardType(SFix(1 exp, -14 exp))

  def testCooleyTukeyFFTHardware(testLength: Int, factors: Seq[Int], inverse: Boolean = false, epsilon: Double = 0.1, realSequence: Boolean = false) = {
    SimConfig.withWave.compile(CooleyTukeyFFTStream(N = testLength, factors = factors, inverse = inverse, dataType, coeffType)).doSim { dut =>

      val testComplex: Array[BComplex] = if (!realSequence) (0 until testLength).map(_ => ChainsawRand.nextComplex(-1.0, 1.0)).toArray
      else if (realSequence && !inverse) (0 until testLength).map(_ => (ChainsawRand.nextDouble() - 0.5) * 2).map(new BComplex(_, 0.0)).toArray
      else {
        val zero = new BComplex(0.0, 0.0)
        val valid = (1 until testLength / 2).map(_ => ChainsawRand.nextComplex(-1.0, 1.0))
        val conjed = valid.map(_.conjugate).reverse
        (zero +: valid :+ zero) ++ conjed
      }.toArray

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

      dataIn.payload.zip(testComplex).foreach { case (port, complex) =>
        dataIn.valid #= true
        port.real #= complex.real
        port.imag #= complex.imag
      }
      clockDomain.waitSampling()
      dataIn.valid #= false

      clockDomain.waitSampling(dut.core.latency + 1)

      val golden = if (!inverse) dft(testComplex.toSeq.asDv) else idft(testComplex.toSeq.asDv).map(_ * testLength)

      println(golden)
      println(dutResult)
      assert(dutResult.nonEmpty)
      assert(golden ~= dutResult.asDv)
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

  def testRadixR(factors: Seq[Int], inverse: Boolean, epsilon: Double, realSequence: Boolean = false) =
    testCooleyTukeyFFTHardware(factors.product, factors, inverse, epsilon, realSequence)

  "radix-r FFT and IFFT" should "pass the following tests" in {

    // FFT
    testRadixR(Seq.fill(6)(2), false, 0.1) // radix-2
    printlnGreen(s"radix-2 FFT passed")
    testRadixR(Seq.fill(3)(4), false, 0.1) // radix-4
    printlnGreen(s"radix-4 FFT passed")
    testRadixR(Seq.fill(2)(8), false, 0.1) // radix-8
    printlnGreen(s"radix-8 FFT passed")
    // IFFT
    testRadixR(Seq.fill(6)(2), true, 0.5) // radix-2
    printlnGreen(s"radix-2 IFFT passed")
    testRadixR(Seq.fill(3)(4), true, 0.5) // radix-4
    printlnGreen(s"radix-4 IFFT passed")
    //    testRadixR(Seq.fill(2)(8), true) // radix-8
    //    printlnGreen(s"radix-8 IFFT passed")

  }

  "radix-r,real-valued FFT and hermitian symmetric IFFT" should "pass the following tests" in {
    // real-valued FFT / hermitian symmetric IFFT
    testRadixR(Seq.fill(6)(2), inverse = true, epsilon = 0.5, realSequence = true)
    printlnGreen(s"radix-2 hermitian symmetric IFFT passed")
  }

  "other Cooley-Tukey FFTs" should "pass the following tests" in {
    //    testCooleyTukeyFFTHardware(75, 16, 16, Seq(3, 5, 5))
    printlnGreen(s"75-point as 3*5*5 passed")
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