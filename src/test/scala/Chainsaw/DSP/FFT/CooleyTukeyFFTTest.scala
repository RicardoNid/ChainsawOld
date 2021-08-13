package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class CooleyTukeyFFTTest extends AnyFunSuite {

  def testCooleyTukeyFFTHardware(testLength: Int, dataWidth: Int, coeffWidth: Int, factors: Seq[Int]) = {
    SimConfig.withWave.compile(new CooleyTukeyFFT(N = testLength, dataWidth = dataWidth, coeffWidth = coeffWidth, factors = factors)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)

      val test = (0 until 2 * testLength).map(_ => (DSPRand.nextDouble() - 0.5) * 1)
      val testComplex = (0 until testLength).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray
      val golden = eng.feval[Array[MComplex]]("fft", testComplex)

      (0 until dut.N).foreach { i =>
        dataIn(2 * i) #= testComplex(i).real
        dataIn(2 * i + 1) #= testComplex(i).imag
      }
      clockDomain.waitSampling(20)

      val goldenDouble = golden.map(complex => Seq(complex.real, complex.imag)).flatten
      val yoursDouble = dataOut.map(_.toDouble())
      val epsilon = 0.5
      println(s"golden: ${golden.map(complex => Seq(complex.real, complex.imag)).flatten.map(_.toString.take(6).padTo(6, '0')).mkString(" ")}")
      println(s"yours:  ${dataOut.map(_.toDouble.toString.take(6).padTo(6, '0')).mkString(" ")}")
      assert(goldenDouble.zip(yoursDouble).forall { case (d, d1) => (d - d1).abs < epsilon })
    }
  }

  test("testRadixRFFT") {

    def testRadixR: Seq[Int] => Unit = testCooleyTukeyFFTHardware(64, 16, 16, _)

    testRadixR(Seq.fill(6)(2)) // radix-2
    printlnGreen(s"radix-2 FFT passed")
    testRadixR(Seq.fill(3)(4)) // radix-4
    printlnGreen(s"radix-4 FFT passed")
    testRadixR(Seq.fill(2)(8)) // radix-8
    printlnGreen(s"radix-8 FFT passed")

  }
}


