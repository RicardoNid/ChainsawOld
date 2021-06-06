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
  val N = UInt(lN bits)
  val RhoSquare = UInt(lN bits)
  val omega = UInt(lN bits)
}

// first version, design with single, big multiplier
class MontExp(lN: Int) extends DSPDUTTiming[MontExpInput, UInt] {
  override val input: MontExpInput = in(MontExpInput(lN))
  override val output: UInt = out(Reg(UInt(lN bits)))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)

  // componnets
  val parameterRegs = Reg(MontExpInput(lN))
  // the second most importatnt exponent bit is the decisive one
  val currentExponentBit = parameterRegs.expononet(lN - 2)

  val initDataRegs = Reg(UInt(lN bits)) // regs for "aMont"

  val onGoingDataRegs = Reg(UInt(2 * lN bits)) // regs for "reg"
  val onGoingHigh = onGoingDataRegs(2 * lN - 1 downto lN)
  val onGoingLow = onGoingDataRegs(lN - 1 downto 0)

  val tempRegs = Reg(UInt(lN bits)) // regs for "reg"

  val mult = new BigMult(lN)

  val innerCounter = Counter(lN * 2)

  val exponentCounter = Counter(lN)
  val exponentEnd = exponentCounter.valueNext === parameterRegs.expononet

  // datapath
  val op0 = UInt(lN bits)
  val op1 = UInt(lN bits)
  val prod = UInt(2 * lN bits)
  val prodHigh = prod(2 * lN - 1 downto lN)
  val prodLow = prod(lN - 1 downto 0)
  mult.input(0) := op0
  mult.input(1) := op1
  prod := mult.output

  op0 := U(0, lN bits)
  op1 := U(0, lN bits)

  // << 1 leads to on bit more, resize would take lower(right) bits
  def exponentMove() = parameterRegs.expononet := (parameterRegs.expononet << 1).resized

  // template of montRedc subprocedure
  def montRedcOnData(regForModedInput: UInt, regForFullInput: UInt,
                     regForU: UInt,
                     regsForOut: Vec[UInt], init: Int) = {
    when(innerCounter.value === U(init)) { // second cycle, first mult of montRedc
      op0 := regForModedInput // t mod Rho
      op1 := parameterRegs.omega
      regForU := prodLow // U
    }.elsewhen(innerCounter.value === U(init + 1)) {
      op0 := regForU // U
      op1 := parameterRegs.N // N
      // FIXME: an implicit 2 * lN-bit adder, fix it
      // TODO: this should be part of the datapath
      // this is the only place where 2 * lN-bit length is needed
      val mid = (regForFullInput + prod) >> lN // (t + U * N) / Rho
      regsForOut.foreach(_ := mid)
      // FIXME: add the reduction circuit
    }
  }

  def montMultOnData(input0: UInt, input1: UInt) = {
    when(innerCounter.value === U(0)) { // first cycle, square
      op0 := input0 // aMont ^ 2n / aMont ^ n
      op1 := input1 // aMont / aMont ^ n
      onGoingDataRegs := prod // aMont ^ (2n+1) / AMont ^ (2n)
    }
    montRedcOnData(onGoingLow, onGoingDataRegs, tempRegs, Vec(onGoingLow), 1)
  }

  val fsm = new StateMachine {
    val INIT = new StateDelay(1) with EntryPoint
    val PRE = new StateDelay(2)
    val DoSquareFor1 = new StateDelay(3)
    val DoMultFor1 = new StateDelay(3)
    val DoSquareFor0 = new StateDelay(3)
    val POST = new StateDelay(2)

    states.foreach(_.whenIsActive(innerCounter.increment()))
    states.foreach(_.onExit(innerCounter.clear()))

    // state trasition
    INIT.whenCompleted(goto(PRE))
    PRE.whenCompleted {
      exponentCounter.clear()
      when(currentExponentBit)(goto(DoSquareFor1))
        .otherwise(goto(DoSquareFor0))
    }
    DoSquareFor1.whenCompleted {
      exponentMove()
      goto(DoMultFor1)
    }
    DoMultFor1.whenCompleted(goto(DoMultFor1)).whenCompleted {
      exponentMove()
      exponentCounter.increment()
      when(exponentEnd)(goto(POST))
        .elsewhen(currentExponentBit)(goto(DoSquareFor1))
        .otherwise(goto(DoSquareFor0))
    }
    DoSquareFor0.whenCompleted {
      exponentMove()
      exponentCounter.increment()
      when(exponentEnd)(goto(POST))
        .elsewhen(currentExponentBit)(goto(DoSquareFor1))
        .otherwise(goto(DoSquareFor0))
    }
    POST.whenCompleted(goto(INIT))

    // state workload on datapath
    // TODO: merge INIT workload with PRE ?
    INIT.whenIsActive(parameterRegs := input) // data initialization
    PRE.whenIsActive(montRedcOnData(parameterRegs.value, parameterRegs.value, initDataRegs, Vec(initDataRegs, onGoingLow), 0))
    DoSquareFor1.whenIsActive(montMultOnData(onGoingLow, onGoingLow))
    DoMultFor1.whenIsActive(montMultOnData(onGoingLow, initDataRegs))
    DoSquareFor0.whenIsActive(montMultOnData(onGoingLow, onGoingLow))
    POST.whenIsActive(montRedcOnData(onGoingLow, onGoingLow, tempRegs, Vec(output), 0))
  }

}

object Experiment {
  def main(args: Array[String]): Unit = {
    //    GenRTL(new BigMult(512))
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