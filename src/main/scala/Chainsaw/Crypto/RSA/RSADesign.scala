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

  val RhoSquare = UInt(lN bits)
  val omega = UInt(lN bits)

  val value = UInt(lN bits)
}

// first version, design with single, big multiplier
class MontExp(lN: Int) extends DSPDUTTiming[MontExpInput, UInt] {
  override val input: MontExpInput = in(MontExpInput(lN))
  override val output: UInt = out(Reg(UInt(lN bits)))
  override val timing: TimingInfo = TimingInfo(1, 1, 2, 1)

  // components
  val parameterRegs = Reg(MontExpInput(lN))
  // the second most importatnt exponent bit is the decisive one
  val currentExponentBit = parameterRegs.exponent(lN - 2)

  val innerCounter = Counter(lN)

  val exponentCounter = Counter(lN)
  val exponentEnd = exponentCounter.valueNext === (parameterRegs.exponentLength - 1)

  // montmul datapath
  val mult = new BigMult(lN)
  val add = new BigAdd(2 * lN)
  val sub = new BigSub(lN + 1)

  val aMontRegs = Reg(UInt(lN bits)) // regs for "aMont"
  val readToAMontRegs = RegInit(False)

  val prodRegs = Reg(UInt(2 * lN bits)) // regs for "reg"

  // multiplier ports
  val multOp0 = UInt(lN bits)
  val multOp1 = UInt(lN bits)
  val prod = UInt(2 * lN bits)
  def prodHigh = prod(2 * lN - 1 downto lN) // caution: val would lead to problems
  def prodLow = prod(lN - 1 downto 0)

  // multiplier is used for many different purposes
  mult.input(0) := multOp0
  mult.input(1) := multOp1
  prod := mult.output // prod acts like a register as mult is end-registered
  prod.simPublic()
  multOp0 := U(0, lN bits)
  multOp1 := U(0, lN bits)

  // add and sub are used for dedicated purpose
  // following signals are valid when inner counter points to 0
  add.input(0) := prodRegs // t for the montRed(aMont * bMont for the montMul)
  add.input(1) := prod // U * N
  // TODO: cautions!
  sub.input(0) := add.output(2 * lN downto lN).asSInt // mid = (t + U * N) / Rho, lN+1 bits
  sub.input(1) := parameterRegs.N.intoSInt // N, padded to lN + 1 bits
  val det = sub.output
  val montRedcRet = Mux(det >= S(0), modRho(det).asUInt, add.output(2 * lN - 1 downto lN))

  when(readToAMontRegs) {
    aMontRegs := montRedcRet
    readToAMontRegs := False
  }

  // utilities and subroutines
  // << 1 leads to on bit more, resize would take lower(right) bits
  def modRho[T <: BitVector](value: T) = value(lN - 1 downto 0)
  def divideRho(value: UInt) = value >> lN
  def exponentMove() = parameterRegs.exponent := (parameterRegs.exponent << 1).resized

  def montMulOnData(input0: UInt, input1: UInt) = {
    when(innerCounter.value === U(0)) { // first cycle, square
      multOp0 := input0 // aMont ^ 2n / aMont ^ n
      multOp1 := input1 // aMont / aMont ^ n
    }
    montRedOnData(prod, 1)
  }

  // template of montRedc subprocedure
  def montRedOnData(input: UInt, // 2 * lN bits
                    init: Int) = {
    when(innerCounter.value === U(init)) { // second cycle, first mult of montRedc
      multOp0 := modRho(input) // t mod Rho
      multOp1 := parameterRegs.omega
      prodRegs := prod // full t
    }.elsewhen(innerCounter.value === U(init + 1)) {
      multOp0 := prodLow // U
      multOp1 := parameterRegs.N // N
    }
  }

  val fsm = new StateMachine {
    // state declarations
    val INIT = new StateDelayFixed(1) with EntryPoint // FIXME: this lasts for 3 cycles
    //    val PRECOM = new StateDelayFixed(lN) // precomputation of omega and RhoSquare
    val PRE = new StateDelayFixed(3)
    val DoSquareFor1 = new StateDelayFixed(3)
    val DoMultFor1 = new StateDelayFixed(3)
    val DoSquareFor0 = new StateDelayFixed(3)
    val POST = new StateDelayFixed(5)

    val allStates = Seq(INIT, PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST)
    allStates.foreach(_.whenIsActive(innerCounter.increment()))
    allStates.foreach(_.whenCompleted(innerCounter.clear()))

    val stateTransition = new Area {
      // state transitions and counter maintenances
      INIT.whenCompleted(goto(PRE))
      //    INIT.whenCompleted(goto(PRECOM))
      //    PRECOM.whenCompleted(goto(PRE))
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

    // TODO: remove these later
    DoSquareFor0.cache.value.simPublic()

    val stateFlags = states.map(isActive(_))
    stateFlags.foreach(_.simPublic())

    val isPRE = isActive(PRE)
    val isPOST = isActive(POST)
    val isINIT = isActive(INIT)
    val isBOOT = isActive(stateBoot)

    // state workload on datapath
    // TODO: merge INIT workload with PRE ?
    INIT.whenIsActive(parameterRegs := input) // data initialization
    //    PRECOM.whenIsActive {}
    PRE.whenIsActive(montMulOnData(parameterRegs.value, parameterRegs.RhoSquare))
    PRE.whenCompleted(readToAMontRegs.set()) // extra work
    DoSquareFor1.whenIsActive(montMulOnData(montRedcRet, montRedcRet))
    DoMultFor1.whenIsActive(montMulOnData(montRedcRet, aMontRegs))
    DoSquareFor0.whenIsActive(montMulOnData(montRedcRet, montRedcRet))
    POST.whenIsActive {
      montMulOnData(montRedcRet, U(1))
      when(innerCounter.value === U(3))(output := montRedcRet)
    }
  }
}

