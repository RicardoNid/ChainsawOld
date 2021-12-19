package Chainsaw.DSP.interleave

import Chainsaw._
import Chainsaw.dspTest._
import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

class AdaptiveMatIntrlvTest extends AnyFlatSpec {

  behavior of "AdaptiveMatIntrlvTest"

  // TODO: a well-designed test for different I/O modes
  it should "work" in {

  }

  it should "synth" in {
    VivadoSynth(
      new Component with DSPTestable[Vec[Bits], Vec[Bits]] {
        val core = AdaptiveMatIntrlv(64, 256, 1024, 1024, HardType(Bits(1 bits)))
        override val dataIn = slave(cloneOf(core.dataIn))
        override val dataOut = master(cloneOf(core.dataOut))
        override val latency = core.latency + 2
        dataIn.m2sPipe() >> core.dataIn
        core.dataOut.m2sPipe() >> dataOut
      }
    )
  }

}
