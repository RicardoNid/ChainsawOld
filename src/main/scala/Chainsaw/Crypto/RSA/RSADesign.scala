package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

case class MontExpInput(lN: Int) extends Bundle {
  val N = UInt(lN bits)
  val exponent = UInt(lN bits)
  val exponentLength = UInt(log2Up(lN) + 1 bits) // the exponent can be lN-bit long
  val value = UInt(lN bits)
}

// first version, design with single, big multiplier
class MontExp(lN: Int) extends DSPDUTTiming[MontExpInput, UInt] {
  override val input: MontExpInput = in(MontExpInput(lN))
  override val output: UInt = out(Reg(UInt(lN bits)))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)

  // operator modules
  val mult = new BigMult(lN)
  val add = new BigAdd(2 * lN)
  val sub = new BigSub(lN + 1)
  // preassign to avoid latches
  mult.input(0) := U(0, lN bits)
  mult.input(1) := U(0, lN bits)
  add.input(0) := U(0, 2 * lN bits)
  add.input(1) := U(0, 2 * lN bits)
  sub.input(0) := S(0, lN + 1 bits)
  sub.input(1) := S(0, lN + 1 bits)

  // design parameters
  val pipelineFactor = mult.latency
  val precomCycles = (lN + 2) * pipelineFactor

  // components
  // regs for data
  val inputRegs = Reg(MontExpInput(lN))
  val singleLengthDataIn = UInt(lN bits)
  val singleLengthDataOut = Delay(singleLengthDataIn, 1)
  val singleLengthReg = Reg(UInt(lN bits)) // regs for "aMont"
  val doubleLengthReg = Reg(UInt(2 * lN bits)) // regs for "reg"
  def doubleLengthRegLow = doubleLengthReg(lN - 1 downto 0)
  val omegaRegs = Reg(UInt(lN bits))
  val rhoSquareReg = Reg(UInt(lN bits))
  // TODO: improve this
  val modMask = RegInit(B(BigInt(3), lN bits)) // mask for mod in getOmega progress
  val addMask = RegInit(B(BigInt(2), lN bits)) // mask for add in getOmega progress
  // counters
  val pipelineCounter = Counter(pipelineFactor)
  val innerCounter = Counter(precomCycles) // counter used inside every StateDelay for control
  val exponentCounter = Counter(lN) // counter
  // flags
  val currentExponentBit = inputRegs.exponent(lN - 2) // the second most importatnt exponent bit is the decisive one
  val exponentEnd = exponentCounter.valueNext === (inputRegs.exponentLength - 1)
  val readRet = RegInit(False) // flag
  when(readRet) {
    singleLengthReg := reductionRet
    readRet := False
  }

  // mult.output acts like a register as mult is end-registered
  def prodHigh = mult.output(2 * lN - 1 downto lN) // caution: val would lead to problems
  def prodLow = mult.output(lN - 1 downto 0)

  // following signals are valid when inner counter points to 0
  // datapath of the reduction
  val det = sub.output
  val reductionRet = Mux(det >= S(0), modRho(det).asUInt, sub.input(0)(lN - 1 downto 0).asUInt)

  // utilities and subroutines
  // << 1 leads to on bit more, resize would take lower(right) bits
  def modRho[T <: BitVector](value: T) = value(lN - 1 downto 0)
  def divideRho(value: UInt) = value >> lN
  def exponentMove() = inputRegs.exponent := (inputRegs.exponent << 1).resized

  def montMulDatapath(input0: UInt, input1: UInt) = {
    when(innerCounter.value === U(0)) { // first cycle, square
      mult.input(0) := input0 // aMont ^ 2n / aMont ^ n
      mult.input(1) := input1 // aMont / aMont ^ n
      addSubDatapath()
    }
    montRedDatapath(mult.output, 1)
    when(innerCounter.value === U(3))(addSubDatapath())

    def montRedDatapath(input: UInt, // 2 * lN bits
                        init: Int) = {
      when(innerCounter.value === U(init)) { // second cycle, first mult of montRedc
        mult.input(0) := modRho(input) // t mod Rho
        mult.input(1) := omegaRegs
        doubleLengthReg := mult.output // full t
      }.elsewhen(innerCounter.value === U(init + 1)) {
        mult.input(0) := prodLow // U
        mult.input(1) := inputRegs.N // N
      }
    }

    def addSubDatapath() = {
      add.input(0) := doubleLengthReg // t for the montRed(aMont * bMont for the montMul)
      add.input(1) := mult.output // U * N
      // TODO: cautions!
      sub.input(0) := add.output(2 * lN downto lN).asSInt // mid = (t + U * N) / Rho, lN+1 bits
      sub.input(1) := inputRegs.N.intoSInt // N, padded to lN + 1 bits
    }
  }

  def getOmegaDatapath() = {

    def maskMove() = {
      modMask := modMask(lN - 2 downto 0) ## True
      addMask := (addMask << 1).resized
    }

    when(innerCounter.value === U(0)) { // starts from f(1) = 0 mod 2
      mult.input(0) := U(1) // original solution 1
      mult.input(1) := inputRegs.N
      doubleLengthReg(lN - 1 downto 0) := mult.input(0) // save the solution
    }.elsewhen(innerCounter.value === U(lN)) {
      add.input(0) := (~doubleLengthRegLow).resized
      add.input(1) := U(1)
      omegaRegs := add.output(lN - 1 downto 0)
    }.otherwise {
      mult.input(0) :=
        Mux((prodLow & modMask.asUInt) === U(1),
          doubleLengthRegLow, // solution
          doubleLengthRegLow | addMask.asUInt) // solution + 1 << exp
      mult.input(1) := inputRegs.N
      doubleLengthReg(lN - 1 downto 0) := mult.input(0) // save the solution
      maskMove()
    }
  }

  def getRhoSquareDatapath() = {
    when(innerCounter.value === 0) {
      sub.input(0) := S(BigInt(1) << (lN - 1))
      sub.input(1) := inputRegs.N.intoSInt
      singleLengthReg := reductionRet
    }.otherwise {
      sub.input(0) := (singleLengthReg << 1).asSInt
      sub.input(1) := inputRegs.N.intoSInt
      singleLengthReg := reductionRet
    }
  }


  val fsm = new StateMachine {
    // state declarations
    //    val INIT = new StateDelayFixed(pipelineFactor) with EntryPoint
    val INIT = new State with EntryPoint // FIXME: this doesn't work for 1 cycles
    val PRECOM = new StateDelayFixed(precomCycles) // precomputation of omega and RhoSquare
    val PRE = new StateDelayFixed(3 * pipelineFactor)
    val DoSquareFor1 = new StateDelayFixed(3 * pipelineFactor)
    val DoMultFor1 = new StateDelayFixed(3 * pipelineFactor)
    val DoSquareFor0 = new StateDelayFixed(3 * pipelineFactor)
    val POST = new StateDelayFixed(5 * pipelineFactor)

    // the overall control strategy: by the innerCounter
    val allStateDelays = Seq(PRECOM, PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST)
    allStateDelays.foreach(_.whenIsActive {
      pipelineCounter.increment()
      when(pipelineCounter.willOverflow)(innerCounter.increment())
    })
    allStateDelays.foreach(_.whenCompleted(innerCounter.clear()))

    val stateTransition = new Area { // state transitions and counter maintenances
      INIT.whenIsActive(goto(PRECOM))
      PRECOM.whenCompleted(goto(PRE))
      PRE.whenCompleted {
        exponentMove()
        exponentCounter.clear()
        when(currentExponentBit)(goto(DoSquareFor1))
          .otherwise(goto(DoSquareFor0))
      }
      DoSquareFor1.whenCompleted(goto(DoMultFor1))
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
    }

    // TODO: merge INIT workload with PRECOM ?
    val workload = new Area { // state workload on datapath
      INIT.whenIsActive(inputRegs := input) // data initialization
      PRECOM.whenIsActive { // computing omega and rhoSquare
        getRhoSquareDatapath()
        getOmegaDatapath()
      }
      PRECOM.whenCompleted {
        rhoSquareReg := reductionRet
      } // store omega and rhoSquare
      PRE.whenIsActive(montMulDatapath(inputRegs.value, rhoSquareReg))
      PRE.whenCompleted(readRet.set()) // extra work
      DoSquareFor1.whenIsActive(montMulDatapath(reductionRet, reductionRet))
      DoMultFor1.whenIsActive(montMulDatapath(reductionRet, singleLengthReg))
      DoSquareFor0.whenIsActive(montMulDatapath(reductionRet, reductionRet))
      POST.whenIsActive {
        montMulDatapath(reductionRet, U(1))
        when(innerCounter.value === U(3))(output := reductionRet)
      }
    }

    // signals for simulation
    val stateFlags = states.map(isActive(_))
    stateFlags.foreach(_.simPublic())

    val isPRE = isActive(PRE)
    val isPOST = isActive(POST)
    val isINIT = isActive(INIT)
    val isPRECOM = isActive(PRECOM)
    val isBOOT = isActive(stateBoot)
  }
}

object MontExp {
  def main(args: Array[String]): Unit = {
    VivadoSynth(new MontExp(512))
    //    VivadoElabo(new MontExp(512))
  }
}

