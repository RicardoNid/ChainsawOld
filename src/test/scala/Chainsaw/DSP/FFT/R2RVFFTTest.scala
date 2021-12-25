package Chainsaw.DSP.FFT

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.algos.MatlabRefs.dft
import Chainsaw.matlabIO._

class R2RVFFTTest extends AnyFlatSpec with Matchers {

  val testSize = 64
  val dataType = HardType(SFix(7 exp, -8 exp))
  val coeffType = HardType(SFix(1 exp, -11 exp))

  SimConfig.withWave.compile(R2RVFFT(testSize, dataType, coeffType)).doSim { dut =>
    import dut.{dataIn, dataOut, clockDomain}

    clockDomain.forkStimulus(2)
    dataIn.valid #= false
    clockDomain.waitSampling()

    val testCase = (0 until testSize).map(_ => (ChainsawRand.nextDouble() - 0.5) * 2)
    dataIn.payload.zip(testCase).foreach { case (fix, d) => fix #= d }
    dataIn.valid #= true
    clockDomain.waitSampling()

    dataIn.valid #= false
    clockDomain.waitSampling(dut.latency)

    println(s"latency = ${dut.latency}")
    println(dft(testCase.map(BComplex(_)).toDv))
  }
}
