package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class MontExpInput(lN: Int) extends Bundle {
  val N = UInt(lN bits)
  val exponent = UInt(lN bits)
  val exponentLength = UInt(log2Up(lN) + 1 bits) // the exponent can be lN-bit long
  val value = UInt(lN bits)
}

// first version, design with single, big multiplier
class MontExp(lN: Int, mulLatency: Int = 4, addLatency: Int = 0) extends DSPDUTTiming[MontExpInput, UInt] {
  override val input: MontExpInput = in(MontExpInput(lN))
  override val output: UInt = out(UInt(lN bits))
  output := U(0)
  val valid = out(RegInit(False))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)

  // operator modules
  val mult = new BigMult(lN, mulLatency)
  val add = new BigAdd(2 * lN, addLatency)
  val sub = new BigSub(lN + 1, addLatency)
  // preassign to avoid latches
  mult.input(0) := U(0, lN bits)
  mult.input(1) := U(0, lN bits)
  add.input(0) := U(0, 2 * lN bits)
  add.input(1) := U(0, 2 * lN bits)
  sub.input(0) := S(0, lN + 1 bits)
  sub.input(1) := S(0, lN + 1 bits)

  // design parameters
  val pipelineFactor = mulLatency + 2 * addLatency
  val precomCycles = (lN + 2) * pipelineFactor

  // components
  // regs for data
  val inputValueRegs = Vec(Reg(UInt(lN bits)), pipelineFactor) // would be reused for aMont
  val exponentReg = Reg(UInt(lN bits))
  val exponentLengthReg = Reg(UInt(log2Up(lN) + 1 bits))
  val NReg = Reg(UInt(lN bits))
  val singleLengthQueue = StreamFifo(UInt(lN bits), pipelineFactor + 1) // general queue for intermediate data
  initFIFO(singleLengthQueue)
  val doubleLengthQueue = StreamFifo(UInt(2 * lN bits), pipelineFactor + 1) // general queue for intermediate data
  initFIFO(doubleLengthQueue)
  doubleLengthQueue.io.pop.ready.allowOverride
  val omegaRegs = Reg(UInt(lN bits))
  val rhoSquareReg = Reg(UInt(lN bits))
  // TODO: improve this
  val modMask = RegInit(B(BigInt(3), lN bits)) // mask for mod in getOmega progress
  val addMask = RegInit(B(BigInt(2), lN bits)) // mask for add in getOmega progress
  // counters
  val pipelineCounter = Counter(pipelineFactor)
  val operationCounter = Counter(precomCycles) // counter used inside every StateDelay for control
  val exponentCounter = Counter(lN) // counter
  // flags
  val currentExponentBit = exponentReg(lN - 2) // the second most importatnt exponent bit is the decisive one
  val exponentEnd = exponentCounter.valueNext === (exponentLengthReg - 1)
  val saveAMont = RegInit(False) // flag

  // following signals are valid when inner counter points to 0
  // datapath of the reduction
  val det = sub.output
  val reductionRet = Mux(det >= S(0), modRho(det).asUInt, modRho(sub.input(0)).asUInt)

  def initFIFO(fifo: StreamFifo[UInt]) = {
    fifo.io.push.valid := False
    fifo.io.push.payload := U(0)
    fifo.io.pop.ready := False
  }
  def push(fifo: StreamFifo[UInt], data: UInt) = {
    fifo.io.push.valid := True
    fifo.io.push.payload := data
  }
  def pop(fifo: StreamFifo[UInt]) = {
    fifo.io.pop.ready := True
    fifo.io.pop.payload
  }
  // utilities and subroutines
  def doubleLengthLow = modRho(pop(doubleLengthQueue))
  // mult.output acts like a register as mult is end-registered
  def prodLow = mult.output(lN - 1 downto 0)
  // << 1 leads to on bit more, resize would take lower(right) bits
  def modRho[T <: BitVector](value: T) = value(lN - 1 downto 0)
  def divideRho(value: UInt) = value >> lN
  //  def exponentMove() = inputRegs.exponent := (inputRegs.exponent << 1).resized
  def exponentMove() = exponentReg := (exponentReg << 1).resized
  def maskMove() = {
    modMask := modMask(lN - 2 downto 0) ## True
    addMask := (addMask << 1).resized
  }
  def pipelineCycle = pipelineCounter.value
  def atOperation(count: Int) = operationCounter.value === U(count)
  def atPipelineCycle(count: Int) = pipelineCycle === U(count)

  def montMulDatapath(input0: UInt, input1: UInt) = {
    when(atOperation(0)) { // first operation a * b
      mult.input(0) := input0 // a
      mult.input(1) := input1 // b
      addSubDatapath()
    }.elsewhen(atOperation(1)) { // second cycle U = t * omega mod rho
      mult.input(0) := prodLow // t mod rho, t = a * b
      mult.input(1) := omegaRegs // omega
      push(doubleLengthQueue, mult.output) // full t
    }.elsewhen(atOperation(2)) { // third operation U * N
      mult.input(0) := prodLow // U, U = t * omega mod rho
      mult.input(1) := NReg // N
    }
  }

  def addSubDatapath() = { // t & UN given, calculate mid and ret
    add.input(0) := pop(doubleLengthQueue) // t for the montRed(aMont * bMont for the montMul)
    add.input(1) := mult.output // U * N
    sub.input(0) := add.output(2 * lN downto lN).asSInt // mid = (t + U * N) / Rho, lN+1 bits
    sub.input(1) := NReg.intoSInt // N, padded to lN + 1 bits
  }

  val getOmegaRunning = Bool() // flag
  getOmegaRunning := False
  val getOmegaDatapath = new Area {
    when(getOmegaRunning) {
      when(atOperation(0)) { // starts from f(1) = 0 mod 2
        mult.input(0) := U(1) // original solution
        mult.input(1) := NReg
        push(doubleLengthQueue, mult.input(0).resized) // save the solution
      }.elsewhen(atOperation(lN)) {
        add.input(0) := (~doubleLengthLow).resized
        add.input(1) := U(1)
        omegaRegs := add.output(lN - 1 downto 0)
      }.otherwise {
        mult.input(0) :=
          Mux((prodLow & modMask.asUInt) === U(1),
            doubleLengthLow, // solution
            doubleLengthLow | addMask.asUInt) // solution + 1 << exp
        mult.input(1) := NReg
        push(doubleLengthQueue, mult.input(0).resized) // save the solution
        when(atPipelineCycle(pipelineFactor - 1))(maskMove())
      }
    }
  }

  def getRhoSquareDatapath() = {
    when(atOperation(0)) {
      sub.input(0) := S(BigInt(1) << (lN - 1))
      sub.input(1) := NReg.intoSInt
      push(singleLengthQueue, reductionRet)
    }.otherwise {
      sub.input(0) := (pop(singleLengthQueue) << 1).asSInt
      sub.input(1) := NReg.intoSInt
      push(singleLengthQueue, reductionRet)
    }
  }

  val fsm = new StateMachine {
    // state declarations
    // FIXME: this doesn't work for cycleCount = 1 OR doesn't compatible with entrypoint
    val INIT = new StateDelayFixed(pipelineFactor) with EntryPoint
    val PRECOM = new StateDelayFixed(precomCycles) // precomputation of omega and RhoSquare
    // states that executes the MontMul
    val PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST = new StateDelayFixed(3 * pipelineFactor)

    // the overall control strategy: by the innerCounter
    val allStateDelays = Seq(INIT, PRECOM, PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST)
    allStateDelays.foreach(_.whenIsActive {
      pipelineCounter.increment()
      when(pipelineCounter.willOverflow)(operationCounter.increment())
    })
    allStateDelays.foreach(_.whenCompleted(operationCounter.clear()))

    val stateTransition = new Area { // state transitions and counter maintenances
      INIT.whenCompleted(goto(PRECOM))
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
      INIT.whenIsActive {
        NReg := input.N
        exponentReg := input.exponent
        exponentLengthReg := input.exponentLength
        inputValueRegs(pipelineCycle) := input.value
        when(atOperation(0))(addSubDatapath()) // to finish the final part of last...
      } // data initialization
      PRECOM.whenIsActive { // computing omega and rhoSquare
        getRhoSquareDatapath()
        getOmegaRunning := True
      }
      PRECOM.whenCompleted {
        rhoSquareReg := reductionRet
      } // store omega and rhoSquare
      PRE.whenIsActive(montMulDatapath(inputValueRegs(pipelineCycle), rhoSquareReg))
      PRE.whenCompleted(saveAMont.set()) // extra work
      when(saveAMont) {
        inputValueRegs(pipelineCycle) := reductionRet
        when(atPipelineCycle(pipelineFactor - 1))(saveAMont := False)
      }
      DoSquareFor1.whenIsActive(montMulDatapath(reductionRet, reductionRet))
      DoMultFor1.whenIsActive(montMulDatapath(reductionRet, inputValueRegs(pipelineCycle)))
      DoSquareFor0.whenIsActive(montMulDatapath(reductionRet, reductionRet))
      POST.whenIsActive(montMulDatapath(reductionRet, U(1)))
      POST.whenCompleted(valid.set())
      when(valid) {
        output := reductionRet
        when(atPipelineCycle(pipelineFactor - 1))(valid.clear())
      }
    }
  }
}

object MontExp {
  def main(args: Array[String]): Unit = {
    VivadoSynth(new MontExp(512))
    //    VivadoElabo(new MontExp(512))
  }
}