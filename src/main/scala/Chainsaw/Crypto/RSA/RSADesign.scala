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
class MontExp(lN: Int, mulLatency: Int = 4, addLatency: Int = 2) extends DSPDUTTiming[MontExpInput, UInt] {
  override val input: MontExpInput = in(MontExpInput(lN))
  override val output: UInt = out(UInt(lN bits))
  output := U(0)
  val valid = out(RegInit(False))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)

  // operator modules
  val mult = new BigMult(lN, mulLatency)
  val add = new BigAdd(2 * lN, addLatency)
  val sub = new BigSub(lN + 1, addLatency)
  val addSub = new BigAddSub(2 * lN, addLatency)

  // preassign to avoid latches
  Seq(mult, addSub).foreach(_.input.foreach(_.clearAll()))
  addSub.isAdd := False

  add.input(0) := U(0, 2 * lN bits)
  add.input(1) := U(0, 2 * lN bits)
  sub.input(0) := S(0, lN + 1 bits)
  sub.input(1) := S(0, lN + 1 bits)

  // design parameters
  val pipelineFactor = mulLatency + 2 * addLatency
  val precomOperations = lN + 2
  val precomCycles = precomOperations * pipelineFactor

  // components
  // regs for data
  val inputValueRegs = Vec(Reg(UInt(lN bits)), pipelineFactor) // would be reused for aMont
  val exponentReg = Reg(UInt(lN bits))
  val exponentLengthReg = Reg(UInt(log2Up(lN) + 1 bits))
  val NReg = Reg(UInt(lN bits))
  // general fifos for intermediate data
  val Seq(bitQueue, singleLengthQueue, anotherSingleLengthQueue, doubleLengthQueue) = // use unapply to declare fifos in batch
    Seq(1, lN, lN + 1, 2 * lN).map(i => FIFO(UInt(i bits), pipelineFactor + 1)) // as they have different width and shared depth
  val fifos = Seq(bitQueue, singleLengthQueue, anotherSingleLengthQueue, doubleLengthQueue)
  fifos.foreach { fifo =>
    fifo.init() // TODO: avoid init in my FIFO
    fifo.io.pop.ready.allowOverride
  }

  val omegaRegs = Reg(UInt(lN bits))
  val rhoSquareReg = Reg(UInt(lN bits))
  // TODO: improve this
  val modMask = RegInit(B(BigInt(3), lN bits)) // mask for mod in getOmega progress
  val addMask = RegInit(B(BigInt(2), lN bits)) // mask for add in getOmega progress
  // counters
  val pipelineCounter = Counter(pipelineFactor)
  val operationCounter = Counter(precomCycles) // counter used inside every StateDelay for control
  val operationCounterAfterMul = Delay(operationCounter.value, mulLatency)
  val operationCounterAfterAdd = Delay(operationCounter.value, addLatency)

  val exponentCounter = Counter(lN) // counter
  // flags
  val currentExponentBit = exponentReg(lN - 2) // the second most importatnt exponent bit is the decisive one
  val exponentEnd = exponentCounter.valueNext === (exponentLengthReg - 1)
  val saveAMont = RegInit(False) // flag

  // following signals are valid when inner counter points to 0
  // datapath of the reduction
  val det = sub.output
  val reductionRet = Mux(det >= S(0), modRho(det).asUInt, modRho(sub.input(0)).asUInt)

  // utilities and subroutines
  def doubleLengthLow = modRho(doubleLengthQueue.pop())
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
  def operationCycle = operationCounter.value

  def atOperation(count: Int) = operationCounter.value === U(count)
  def atOperationAfterMul(count: Int) = operationCounterAfterMul === U(count)
  def atOperationAfterAdd(count: Int) = operationCounterAfterAdd === U(count)

  def atPipelineCycle(count: Int) = pipelineCycle === U(count)
  def montMulDatapath(input0: UInt, input1: UInt) = {
    when(atOperation(0)) { // first operation a * b
      mult.input(0) := input0 // a
      mult.input(1) := input1 // b
      addSubDatapath()
    }.elsewhen(atOperation(1)) { // second cycle U = t * omega mod rho
      mult.input(0) := prodLow // t mod rho, t = a * b
      mult.input(1) := omegaRegs // omega
      doubleLengthQueue.push(mult.output) // full t
    }.elsewhen(atOperation(2)) { // third operation U * N
      mult.input(0) := prodLow // U, U = t * omega mod rho
      mult.input(1) := NReg // N
    }
  }
  def addSubDatapath() = { // t & UN given, calculate mid and ret
    add.input(0) := doubleLengthQueue.pop() // t for the montRed(aMont * bMont for the montMul)
    add.input(1) := mult.output // U * N
    sub.input(0) := add.output(2 * lN downto lN).asSInt // mid = (t + U * N) / Rho, lN+1 bits
    sub.input(1) := NReg.intoSInt // N, padded to lN + 1 bits
  }

  val getOmegaDatapath = new Area {
    val flag = Bool() // flag
    flag.clear()
    val flagAfterMul = Delay(flag, mulLatency, init = False) // not init would lead to leakage
    val flagAfterAdd = Delay(flag, addLatency, init = False)
    when(flag) {
      def newSolution() = Mux(bitQueue.pop().asBool,
        singleLengthQueue.pop(), // solution
        singleLengthQueue.pop() | addMask.asUInt) // solution + (1 << (exp + 1))

      when(atOperation(0)) { // starts from f(1) = 0 mod 2
        mult.doMult(U(1), NReg) // initial solution = 1, 1 * N
        singleLengthQueue.push(U(1).resized) // save the solution
      }.elsewhen(atOperation(precomOperations - 2)) { // lN_0, final solution resolved, wait for the adder tobe available
        singleLengthQueue.push(newSolution())
      }.elsewhen(atOperation(precomOperations - 1)) { // lN+1_0
        addSub.doAdd(~singleLengthQueue.pop(), U(1))
      }.otherwise { // 1_0 ~ lN-1_0
        mult.doMult(newSolution(), NReg) // iteratively go for next solution
        singleLengthQueue.push(newSolution()) // save the solution
        when(atPipelineCycle(pipelineFactor - 1))(maskMove()) // FIXME: separate two mask
      }
    }
    when(flagAfterMul) { // 0_k ~ lN-1_k
      val det = (prodLow & modMask.asUInt) === U(1)
      bitQueue.push(det.asUInt)
    }
    when(flagAfterAdd) {
      when(operationCounterAfterAdd === U(lN + 1))(omegaRegs := modRho(addSub.output)) // lN+2_l
    }
  }

  val getRhoSquareDatapath = new Area {
    val flag = Bool()
    flag.clear()
    val flagAfterAdd = Delay(flag, addLatency, init = False)
    def getRPrime() = {
      val det = anotherSingleLengthQueue.pop() // fetch saved determinant
      val r = modRho(doubleLengthQueue.pop()) // fetch saved solution
      Mux(det.msb, modRho(r << 1), modRho(det)) // r' = 2 * r - N or 2 * r
    }
    when(flag) {
      when(atOperation(0)) {
        addSub.doSub(U(BigInt(1) << lN), NReg) // 2 * r - N
        doubleLengthQueue.push(U(BigInt(1) << (lN - 1))) // r in
      }.elsewhen(atOperation(precomOperations - 1)) {
        rhoSquareReg := getRPrime()
      }.otherwise {
        val rPrime = getRPrime()
        addSub.doSub(rPrime << 1, NReg) // 2 * r' - N
        doubleLengthQueue.push(rPrime.resized) // r' in, save the solution
      }
    }
    when(flagAfterAdd) {
      val det = addSub.output(lN downto 0)
      anotherSingleLengthQueue.push(det)
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
        when(operationCounter.value <= lN + 1)(getRhoSquareDatapath.flag := True)
        getOmegaDatapath.flag := True
      }
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