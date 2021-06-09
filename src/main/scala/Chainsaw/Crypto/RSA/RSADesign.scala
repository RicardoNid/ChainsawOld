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
  override val output: UInt = out(Reg(UInt(lN bits)))
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
  val pipelineFactor = mult.latency
  val precomCycles = (lN + 2) * pipelineFactor

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

  // components
  // regs for data
  //  val inputRegs = Reg(MontExpInput(lN))
  val inputValueRegs = Vec(Reg(UInt(lN bits)), pipelineFactor) // would be reused for aMont
  val exponentReg = Reg(UInt(lN bits))
  val exponentLengthReg = Reg(UInt(log2Up(lN) + 1 bits))
  val NReg = Reg(UInt(lN bits))
  //  val singleLengthReg = Reg(UInt(lN bits)) // regs for "aMont"
  val singleLengthDataIn = Reg(UInt(lN bits))
  val singleLengthDataOut = Delay(singleLengthDataIn, pipelineFactor - 1)
  val singleLengthQueue = StreamFifo(UInt(lN bits), pipelineFactor + 1)
  initFIFO(singleLengthQueue)
  //  val doubleLengthReg = Reg(UInt(2 * lN bits)) // regs for "reg"
  val doubleLengthDataIn = Reg(UInt(2 * lN bits)) // regs for "reg"
  // TODO: optimize
  val doubleLengthDataOut = Delay(doubleLengthDataIn, pipelineFactor - 1) // regs for "reg"
  val doubleLengthQueue = StreamFifo(UInt(2 * lN bits), pipelineFactor + 1)
  initFIFO(doubleLengthQueue)
  doubleLengthQueue.io.pop.ready.allowOverride
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
  //  val currentExponentBit = inputRegs.exponent(lN - 2) // the second most importatnt exponent bit is the decisive one
  val currentExponentBit = exponentReg(lN - 2) // the second most importatnt exponent bit is the decisive one
  //  val exponentEnd = exponentCounter.valueNext === (inputRegs.exponentLength - 1)
  val exponentEnd = exponentCounter.valueNext === (exponentLengthReg - 1)
  val readRet = RegInit(False) // flag

  // following signals are valid when inner counter points to 0
  // datapath of the reduction
  val det = sub.output
  val reductionRet = Mux(det >= S(0), modRho(det).asUInt, sub.input(0)(lN - 1 downto 0).asUInt)

  // utilities and subroutines
  //  def doubleLengthRegLow = doubleLengthReg(lN - 1 downto 0)
  //  def doubleLengthRegLow = doubleLengthDataOut(lN - 1 downto 0)
  def doubleLengthRegLow = modRho(pop(doubleLengthQueue))
  // mult.output acts like a register as mult is end-registered
  def prodHigh = mult.output(2 * lN - 1 downto lN) // caution: val would lead to problems
  def prodLow = mult.output(lN - 1 downto 0)
  // << 1 leads to on bit more, resize would take lower(right) bits
  def modRho[T <: BitVector](value: T) = value(lN - 1 downto 0)
  def divideRho(value: UInt) = value >> lN
  //  def exponentMove() = inputRegs.exponent := (inputRegs.exponent << 1).resized
  def exponentMove() = exponentReg := (exponentReg << 1).resized

  def montMulDatapath(input0: UInt, input1: UInt) = {
    when(innerCounter.value === U(0)) { // first cycle, square
      mult.input(0) := input0 // a
      mult.input(1) := input1 // b
      addSubDatapath()
    }.elsewhen(innerCounter.value === U(1)) { // second cycle, first mult of montRedc
      mult.input(0) := prodLow // t mod rho, t = a * b
      mult.input(1) := omegaRegs
      //        doubleLengthReg := mult.output // full t
      //      doubleLengthDataIn := mult.output // full t
      push(doubleLengthQueue, mult.output) // full t
    }.elsewhen(innerCounter.value === U(2)) {
      mult.input(0) := prodLow // U, U = t * omega mod rho
      //        mult.input(1) := inputRegs.N // N
      mult.input(1) := NReg // N
      //      doubleLengthDataIn := doubleLengthDataOut // keep the full t
    }
    when(innerCounter.value === U(3))(addSubDatapath())

    def addSubDatapath() = {
      //      add.input(0) := doubleLengthReg // t for the montRed(aMont * bMont for the montMul)
      //      add.input(0) := doubleLengthDataOut // t for the montRed(aMont * bMont for the montMul)
      add.input(0) := pop(doubleLengthQueue) // t for the montRed(aMont * bMont for the montMul)
      add.input(1) := mult.output // U * N
      // TODO: cautions!
      sub.input(0) := add.output(2 * lN downto lN).asSInt // mid = (t + U * N) / Rho, lN+1 bits
      //      sub.input(1) := inputRegs.N.intoSInt // N, padded to lN + 1 bits
      sub.input(1) := NReg.intoSInt // N, padded to lN + 1 bits
    }
  }

  def getOmegaDatapath() = {

    def maskMove() = {
      modMask := modMask(lN - 2 downto 0) ## True
      addMask := (addMask << 1).resized
    }

    when(innerCounter.value === U(0)) { // starts from f(1) = 0 mod 2
      mult.input(0) := U(1) // original solution 1
      //      mult.input(1) := inputRegs.N
      mult.input(1) := NReg
      //      doubleLengthReg(lN - 1 downto 0) := mult.input(0) // save the solution
      //      doubleLengthDataIn(lN - 1 downto 0) := mult.input(0) // save the solution
      push(doubleLengthQueue, mult.input(0).resized) // save the solution

    }.elsewhen(innerCounter.value === U(lN)) {
      add.input(0) := (~doubleLengthRegLow).resized
      add.input(1) := U(1)
      omegaRegs := add.output(lN - 1 downto 0)
    }.otherwise {
      mult.input(0) :=
        Mux((prodLow & modMask.asUInt) === U(1),
          doubleLengthRegLow, // solution
          doubleLengthRegLow | addMask.asUInt) // solution + 1 << exp
      //      mult.input(1) := inputRegs.N
      mult.input(1) := NReg
      //      doubleLengthReg(lN - 1 downto 0) := mult.input(0) // save the solution
      //      doubleLengthDataIn(lN - 1 downto 0) := mult.input(0) // save the solution
      push(doubleLengthQueue, mult.input(0).resized) // save the solution
      when(pipelineCounter.value === U(pipelineFactor - 1))(maskMove())
    }
  }

  def getRhoSquareDatapath() = {
    when(innerCounter.value === 0) {
      sub.input(0) := S(BigInt(1) << (lN - 1))
      //      sub.input(1) := inputRegs.N.intoSInt
      sub.input(1) := NReg.intoSInt
      //      singleLengthReg := reductionRet
      //      singleLengthDataIn := reductionRet
      push(singleLengthQueue, reductionRet)
    }.otherwise {
      //      sub.input(0) := (singleLengthReg << 1).asSInt
      //      sub.input(0) := (singleLengthDataOut << 1).asSInt
      sub.input(0) := (pop(singleLengthQueue) << 1).asSInt
      //      sub.input(1) := inputRegs.N.intoSInt
      sub.input(1) := NReg.intoSInt
      //      singleLengthReg := reductionRet
      //      singleLengthDataIn := reductionRet
      push(singleLengthQueue, reductionRet)
    }
  }


  val fsm = new StateMachine {
    // state declarations
    // FIXME: this doesn't work for cycleCount = 1 OR doesn't compatible with entrypoint
    val INIT = new StateDelayFixed(pipelineFactor) with EntryPoint
    val PRECOM = new StateDelayFixed(precomCycles) // precomputation of omega and RhoSquare
    val PRE = new StateDelayFixed(3 * pipelineFactor)
    val DoSquareFor1 = new StateDelayFixed(3 * pipelineFactor)
    val DoMultFor1 = new StateDelayFixed(3 * pipelineFactor)
    val DoSquareFor0 = new StateDelayFixed(3 * pipelineFactor)
    val POST = new StateDelayFixed(5 * pipelineFactor)

    // the overall control strategy: by the innerCounter
    val allStateDelays = Seq(INIT, PRECOM, PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST)
    allStateDelays.foreach(_.whenIsActive {
      pipelineCounter.increment()
      when(pipelineCounter.willOverflow)(innerCounter.increment())
    })
    allStateDelays.foreach(_.whenCompleted(innerCounter.clear()))

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
      //      INIT.whenIsActive(inputRegs := input) // data initialization
      INIT.whenIsActive {
        NReg := input.N
        exponentReg := input.exponent
        exponentLengthReg := input.exponentLength
        inputValueRegs(pipelineCounter.value) := input.value
      } // data initialization
      PRECOM.whenIsActive { // computing omega and rhoSquare
        getRhoSquareDatapath()
        getOmegaDatapath()
      }
      PRECOM.whenCompleted {
        rhoSquareReg := reductionRet
      } // store omega and rhoSquare
      //      PRE.whenIsActive(montMulDatapath(inputRegs.value, rhoSquareReg))
      PRE.whenIsActive(montMulDatapath(inputValueRegs(pipelineCounter.value), rhoSquareReg))
      PRE.whenCompleted(readRet.set()) // extra work
      when(readRet) {
        //    singleLengthReg := reductionRet
        inputValueRegs(pipelineCounter.value) := reductionRet
        when(pipelineCounter.value === U(pipelineFactor - 1))(readRet := False)
      }
      DoSquareFor1.whenIsActive(montMulDatapath(reductionRet, reductionRet))
      //      DoMultFor1.whenIsActive(montMulDatapath(reductionRet, singleLengthReg))
      DoMultFor1.whenIsActive(montMulDatapath(reductionRet, inputValueRegs(pipelineCounter.value)))
      DoSquareFor0.whenIsActive(montMulDatapath(reductionRet, reductionRet))
      POST.whenIsActive {
        montMulDatapath(reductionRet, U(1))
        when(innerCounter.value === U(3))(output := reductionRet)
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