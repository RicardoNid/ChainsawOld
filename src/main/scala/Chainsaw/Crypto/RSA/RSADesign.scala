package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class RSADesign {

}

class BigMult extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(512 bits), 2)
  override val output: UInt = out(RegNext(RegNext(input(0)) * RegNext(input(1))))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)
}

class BigMultMod extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(512 bits), 2)
  val prod = RegNext(input(0)) * RegNext(input(1))
  val ret = prod(511 downto 0)
  override val output: UInt = out(RegNext(ret))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)
}

object Experiment {
  def main(args: Array[String]): Unit = {
    GenRTL(new BigMult)
    //    VivadoSynth(new BigMult)
    // plain design with input and output regs
    // 28168 LUT 1048FF
    // 900 DSP
    // 10.03 MHz
    // xczu7evffvc1156-2 10% LUT 52.08% DSP plain design

    VivadoSynth(new BigMultMod)

    // DSP slice - inner pipeline
  }
}