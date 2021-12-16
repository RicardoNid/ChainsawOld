package Chainsaw.comm.channelEqualizer

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import breeze.numerics.abs
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.mutable.ArrayBuffer

class EqualizationTest extends AnyFlatSpec {

  behavior of "EqualizationTest"

  it should "work" in {
    SimConfig.withWave.compile(Equalization(SFix(7 exp, 18 bits), 1)).doSim { dut =>
      val dutResult = ArrayBuffer[Seq[BComplex]]()
      val dataIn = ChainsawRand.nextComplex()
      val preamble = ChainsawRand.nextComplex()
      dut.clockDomain.forkStimulus(2)
      dut.dataIn.payload.zip(Seq(dataIn)).foreach { case (port, complex) => port #= complex }
      dut.preambleIn.payload.zip(Seq(preamble)).foreach { case (port, complex) => port #= complex }

      dut.clockDomain.waitSampling()
      (0 until 100).foreach { _ =>
        if (dut.counter.value.toInt == dut.period - 1) {
          dut.clockDomain.waitSampling()
          dutResult += dut.dataOut.payload.map(_.toComplex)
        }
        dut.clockDomain.waitSampling()
      }
      println(s"period: ${dut.period}")
      println(s"data: $dataIn")
      println(s"preamble: $preamble")
      println(dataIn / preamble)
      println(dutResult.head.head)
      assert(abs(dataIn / preamble - dutResult.head.head) < 1E-1)
    }
  }

  it should "synth" in {
    VivadoSynth(Equalization(HardType(SFix(7 exp, 18 bits)), 256), "EqualBig")
    VivadoSynth(Equalization(HardType(SFix(7 exp, 18 bits)), 1), "EqualSmall")
  }

}
