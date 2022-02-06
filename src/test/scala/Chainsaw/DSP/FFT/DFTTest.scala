package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import org.scalatest.flatspec.AnyFlatSpec

class DFTTest extends AnyFlatSpec {

  val dataType = HardType(SFix(7 exp, 16 bits))
  val coeffType = HardType(SFix(1 exp, 16 bits))

  "Direct DFT Hardware" should "synth" in {
    val Ns = Seq(2,4,8)
    val inverse = Seq(false, true)
    Seq.tabulate(3, 2) { (i, j) =>
      logger.info(s"test ${Ns(i)}-point DFT, ${if (inverse(j)) "inverse" else "forward"}")
      VivadoSynth(DFT(Ns(i), inverse(j), dataType, coeffType))
    }
  }

  it should "compare DFT-8 with radix-2 FFT impl" in {
    VivadoSynth(DFT(8, true, dataType, coeffType))
    VivadoSynth(CooleyTukeyFFT(8, true, dataType, coeffType, factors = Seq(2, 2, 2)))
  }

  it should "work" in {

  }

}
