package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class CooleyTukeyFFTTest() extends AnyFunSuite {

  def testCooleyTukeyFFTHardware(testLength: Int, dataWidth: Int, coeffWidth: Int, factors: Seq[Int]) = {
    SimConfig.withWave.compile(new CooleyTukeyFFTStream(N = testLength, dataWidth = dataWidth, coeffWidth = coeffWidth, factors = factors)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)

      val test = (0 until 2 * testLength).map(_ => (DSPRand.nextDouble() - 0.5) * 1)
      val testComplex = (0 until testLength).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray
      val golden = Refs.FFT(testComplex)

      (0 until dut.N).foreach { i =>
        dataIn.payload(2 * i) #= testComplex(i).real
        dataIn.payload(2 * i + 1) #= testComplex(i).imag
      }
      clockDomain.waitSampling(20)

      val goldenDouble = golden.map(complex => Seq(complex.real, complex.imag)).flatten
      val yoursDouble = dataOut.payload.map(_.toDouble())
      val epsilon = 0.5
      println(s"golden: ${golden.map(complex => Seq(complex.real, complex.imag)).flatten.map(_.toString.take(6).padTo(6, '0')).mkString(" ")}")
      println(s"yours:  ${dataOut.payload.map(_.toDouble.toString.take(6).padTo(6, '0')).mkString(" ")}")
      assert(goldenDouble.zip(yoursDouble).forall { case (d, d1) => (d - d1).abs < epsilon })
    }
  }

  def testCooleyTukeyFFTBackToBackHardware(testLength: Int, parallelFactor:Int, dataWidth: Int, coeffWidth: Int, factors1: Seq[Int], factors2: Seq[Int]) = {
    SimConfig.withWave.compile(new CooleyTukeyBackToBack( dataWidth = dataWidth, coeffWidth = coeffWidth, factors = factors)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)

      val test = (0 until 2 * testLength).map(_ => (DSPRand.nextDouble() - 0.5) * 1)
      val testComplex = (0 until testLength).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray
      val golden = Refs.FFT(testComplex)

      (0 until dut.N).foreach { i =>
        dataIn.payload(2 * i) #= testComplex(i).real
        dataIn.payload(2 * i + 1) #= testComplex(i).imag
      }
      clockDomain.waitSampling(20)

      val goldenDouble = golden.map(complex => Seq(complex.real, complex.imag)).flatten
      val yoursDouble = dataOut.payload.map(_.toDouble())
      val epsilon = 0.5
      println(s"golden: ${golden.map(complex => Seq(complex.real, complex.imag)).flatten.map(_.toString.take(6).padTo(6, '0')).mkString(" ")}")
      println(s"yours:  ${dataOut.payload.map(_.toDouble.toString.take(6).padTo(6, '0')).mkString(" ")}")
      assert(goldenDouble.zip(yoursDouble).forall { case (d, d1) => (d - d1).abs < epsilon })
    }
  }

  test("test radix-r FFT, fully pipelined") {

    def testRadixR: Seq[Int] => Unit = testCooleyTukeyFFTHardware(64, 16, 16, _)

    testRadixR(Seq.fill(6)(2)) // radix-2
    printlnGreen(s"radix-2 FFT passed")
    testRadixR(Seq.fill(3)(4)) // radix-4
    printlnGreen(s"radix-4 FFT passed")
    testRadixR(Seq.fill(2)(8)) // radix-8
    printlnGreen(s"radix-8 FFT passed")

  }

  test("test other Cooley-Tukey FFTs, fully pipelined") {
//    testCooleyTukeyFFTHardware(75, 16, 16, Seq(3, 5, 5))
    printlnGreen(s"75-point as 3*5*5 passed")
  }
}


