package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

class RadixRFFTTest extends AnyFunSuite {
  test("testRadixRFFT") {
    SimConfig.withWave.compile(new RadixRFFT()).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)

      val test = (0 until 2 * N).map(_ => (DSPRand.nextDouble() - 0.5) * 1)
      val testComplex = (0 until N).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray

      (0 until N).foreach { i =>
        dataIn(2 * i) #= testComplex(i).real
        dataIn(2 * i + 1) #= testComplex(i).imag
      }
      clockDomain.waitSampling(15)

      val eng = AsyncEng.get()

      //      println(testComplex.mkString(" "))
      val golden = eng.feval[Array[MComplex]]("fft", testComplex)

      val goldenDouble = golden.map(complex => Seq(complex.real, complex.imag)).flatten
      val yoursDouble = dataOut.map(_.toDouble())
      val epsilon = 0.5
      println(s"golden: ${golden.map(complex => Seq(complex.real, complex.imag)).flatten.map(_.toString.take(6).padTo(6,'0')).mkString(" ")}")
      println(s"yours:  ${dataOut.map(_.toDouble.toString.take(6).padTo(6,'0')).mkString(" ")}")
      assert(goldenDouble.zip(yoursDouble).forall { case (d, d1) => (d - d1).abs < epsilon })
    }
  }
}
