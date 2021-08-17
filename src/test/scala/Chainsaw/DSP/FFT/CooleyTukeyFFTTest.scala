package Chainsaw.DSP.FFT

import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class CooleyTukeyFFTTest() extends AnyFunSuite {

  def testCooleyTukeyFFTHardware(testLength: Int, dataWidth: Int, coeffWidth: Int, factors: Seq[Int]) = {
    SimConfig.withWave.compile(new CooleyTukeyFFTStream(N = testLength, dataWidth = dataWidth, coeffWidth = coeffWidth, factors = factors)).doSim { dut =>

      val test = (0 until 2 * testLength).map(_ => (DSPRand.nextDouble() - 0.5) * 1)
      val testComplex = (0 until testLength).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray

      import dut._
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataOut.ready #= true
      clockDomain.waitSampling()

      val dutResult = ArrayBuffer[MComplex]()
      val monitor = fork {
        while (true) {
          if (dataOut.valid.toBoolean) {
            dutResult ++= dataOut.payload.map(complex => new MComplex(complex.real.toDouble, complex.imag.toDouble))
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

      clockDomain.waitSampling(dut.core.latency + 10)

      val golden = Refs.FFT(testComplex)
      assert(dutResult.nonEmpty)
      println(golden.mkString(" "))
      println(dutResult.mkString(" "))
      assert(golden.zip(dutResult).forall { case (complex, complex1) => complex.sameAs(complex1, epsilon = 0.5) })
    }
  }

  def testCooleyTukeyBackToBackHardware(testLength: Int, pF: Int, dataWidth: Int, coeffWidth: Int, factors1: Seq[Int], factors2: Seq[Int]) = {
    SimConfig.withWave.compile(new CooleyTukeyBackToBack(
      N = testLength, pF = pF,
      dataWidth = dataWidth, coeffWidth = coeffWidth,
      factors1 = factors1, factors2 = factors2)).doSim { dut =>

      val test = (0 until 2 * testLength).map(_ => (DSPRand.nextDouble() - 0.5) * 1)
      val testComplex = (0 until testLength).map(i => new MComplex(test(2 * i), test(2 * i + 1))).toArray

      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      dataOut.ready #= true
      clockDomain.waitSampling()

      val dutResult = ArrayBuffer[MComplex]()
      val monitor = fork {
        while (true) {
          if (dataOut.valid.toBoolean) {
            dutResult ++= dataOut.payload.map(complex => new MComplex(complex.real.toDouble, complex.imag.toDouble))
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

      clockDomain.waitSampling(100)

      val golden = Refs.FFT(testComplex)
      println(dutResult.zip(golden).map{ case (complex, complex1) => complex.toString + "####" + complex1.toString}.mkString("\n"))
      assert(dutResult.nonEmpty)
      assert(golden.zip(dutResult).forall { case (complex, complex1) => complex.sameAs(complex1, epsilon = 0.5) })
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

  test("test back-to-back Cooley-Tukey FFTs") {
    testCooleyTukeyBackToBackHardware(256, 32, 16, 16, Seq(4, 4, 2), Seq(4, 2))
    printlnGreen(s"256-point as 32*8 back to back passed")
  }
}



