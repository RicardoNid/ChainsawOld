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

class BigMult(lN: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(lN bits), 2)
  override val output: UInt = out(RegNext(RegNext(input(0)) * RegNext(input(1))))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)
}

class BigMultMod(lN: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(lN bits), 2)
  val prod = RegNext(input(0)) * RegNext(input(1))
  val ret = prod(511 downto 0)
  override val output: UInt = out(RegNext(ret))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)
}

case class MontExpInput(lN: Int) extends Bundle {
  val value = UInt(lN bits)
  val expononet = UInt(lN bits)
  val expononetLength = UInt(log2Up(lN) bits)
  val modulus = UInt(lN bits)
  val RhoSquare = UInt(lN bits)
  val omega = UInt(lN bits)
}

// first version, design with single, big multiplier
class MontExp(lN: Int) extends DSPDUTTiming[MontExpInput, UInt] {
  override val input: MontExpInput = in(MontExpInput(lN))
  override val output: UInt = out(UInt(lN bits))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)

  // componnets
  val parameterRegs = Reg(MontExpInput(lN))
  val doMult = RegInit(False)
  val initDataRegs = Reg(UInt(lN bits)) // regs for "aMont"
  val onGoingDataRegs = Reg(UInt(2 * lN bits)) // regs for "reg"
  val onGoingHigh = onGoingDataRegs(2 * lN - 1 downto lN)
  val onGoingLow = onGoingDataRegs(lN - 1 downto 0)
  val mult = new BigMult(lN)
  val innerCounter = Counter(lN * 2)
  //  val mult = new BigMultMod(lN)

  // datapath
  val op0 = UInt(lN bits)
  val op1 = UInt(lN bits)
  val prod = UInt(lN bits)
  mult.input(0) := op0
  mult.input(1) := op1
  prod := mult.output

  parameterRegs := input // !! it works !!
  op0 := input.value
  op1 := input.value
  output := prod

  op0 := U(0, lN bits)
  op1 := U(0, lN bits)

  def montRedcOnData = {

  }

  val fsm = new StateMachine {
    val INIT = new State() with EntryPoint
    val PRE = new State()
    val DoSquareFor1 = new State()
    val DoMultFor1 = new State()
    val DoSquareFor0 = new State()
    val POST = new State()

    states.foreach(_.whenIsActive(innerCounter.increment()))
    states.foreach(_.onExit(innerCounter.clear()))

    INIT.whenIsActive {
      parameterRegs := input
      goto(PRE)
    }
    PRE.whenIsActive {
      when(innerCounter.value === U(0)) { // first cycle
        op0 := parameterRegs.value // t
        op1 := parameterRegs.omega // omega
        initDataRegs := prod(lN - 1 downto 0) // t * omega (mod Rho), named as U
        // expose the second most significant bit where the SM sequence starts from
        parameterRegs.expononet := parameterRegs.expononet << 1
      }.otherwise { // second cycle
        op0 := initDataRegs // U
        op1 := parameterRegs.modulus // N
        // FIXME: an implicit 1024-bit adder, fix it
        // TODO: this should be part of the datapath
        val mid = (prod + parameterRegs.value) >> lN // (t + U * N) / Rho
        initDataRegs := mid
        onGoingLow := mid
        when(parameterRegs.expononet.msb)(goto(DoSquareFor1))
          .otherwise(goto(DoSquareFor0))
      }
    }
    DoSquareFor1.whenIsActive {
      when(innerCounter.value === U(0)) { // first cycle, square
        op0 := onGoingDataRegs // aMont
        op1 := onGoingDataRegs // aMont
        onGoingDataRegs := prod // aMont ^ 2
      }
      when(innerCounter.value === U(1)) { // second cycle, first mult of montRedc
        op0 := onGoingDataRegs // aMont ^ 2
        op1 := parameterRegs.omega
        onGoingDataRegs := prod
      }.elsewhen(innerCounter.value === U(2)) {
        op0 := onGoingDataRegs
        op1 := onGoingDataRegs
      }
    }
  }

}

object Experiment {
  def main(args: Array[String]): Unit = {
    GenRTL(new BigMult(512))
    //    VivadoSynth(new BigMult)
    // plain design with input and output regs
    // 28168 LUT 1048 FF
    // 900 DSP
    // 10.03 MHz
    // xczu7evffvc1156-2 10% LUT 52.08% DSP plain design

    //    VivadoSynth(new BigMultMod(512))
    // plain design(using BigMult and take part of the result) with input and output regs
    // 13795 LUT 533 FF
    // 900 DSP
    // 19.20 MHz

    // DSP slice - inner pipeline
    GenRTL(new MontExp(512))
  }
}