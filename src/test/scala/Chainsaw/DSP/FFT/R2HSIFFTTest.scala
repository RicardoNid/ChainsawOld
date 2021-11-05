package Chainsaw.DSP.FFT

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._

class R2HSIFFTTest extends AnyFlatSpec with Matchers {

  val testSize = 64
  val dataType = HardType(SFix(7 exp, -12 exp))
  val coeffType = HardType(SFix(1 exp, -11 exp))

  "real valued ifft" should "work correctly" in {
    SimConfig.withWave.compile(R2HSIFFT(testSize, dataType, coeffType)).doSim { dut =>
      import dut.{dataIn, clockDomain}

      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      clockDomain.waitSampling()

      val zero = new MComplex(0, 0)
      val valid = zero +: (1 until testSize / 2).map(_ => DSPRand.nextComplex(-1, 1))
      val conjed = valid.tail.map(_.conj).reverse
      val testCase = valid ++ (zero +: conjed)

      dataIn.payload.zip(testCase).foreach { case (number, complex) => number #= complex }
      dataIn.valid #= true
      clockDomain.waitSampling()

      dataIn.valid #= false
      clockDomain.waitSampling(dut.latency)

      println(s"latency = ${dut.latency}")
      //    println(testCase.mkString(" "))
      println(Refs.IFFT(testCase.toArray).map(_.real * testSize / 2)
        .mkString(" "))
      // FIXME: UNACCESSIBLE SIGNAL : (toplevel/dataOut_payload_0_real : out SInt[16 bits]) isn't accessible during the simulation.
      // this test is invalid now
      //    val dutResult = dut.dataOut.payload.map(_.toDouble)
      //    println(dutResult.mkString(" "))
    }

  }
}
