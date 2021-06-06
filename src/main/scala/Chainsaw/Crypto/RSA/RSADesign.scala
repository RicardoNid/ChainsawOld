package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

class RSADesign {

}

// width = 2 * lN
class BigAdd(n: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  override val output: UInt = out(input(0) +^ input(1))
  override val timing: TimingInfo = TimingInfo(1, 1, 0, 1)
}

class BigAddMod(n: Int, m: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  val sum = input(0) +^ input(1)
  override val output: UInt = out(sum(m - 1 downto 0))
  override val timing: TimingInfo = TimingInfo(1, 1, 0, 1)
}

class BigMult(n: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  override val output: UInt = out(RegNext(input(0) * input(1)))
  override val timing: TimingInfo = TimingInfo(1, 1, 1, 1)
}

class BigMultMod(n: Int, m: Int) extends DSPDUTTiming[Vec[UInt], UInt] {
  override val input: Vec[UInt] = in Vec(UInt(n bits), 2)
  val prod = input(0) * input(1)
  val ret = prod(m - 1 downto 0)
  override val output: UInt = out(RegNext(ret))
  override val timing: TimingInfo = TimingInfo(1, 1, 1, 1)
}

case class MontExpInput(lN: Int) extends Bundle {
  val value = UInt(lN bits)
  val expononet = UInt(lN bits)
  val expononetLength = UInt(log2Up(lN) + 1 bits) // the exponent can be lN-bit long
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

  val mult = new BigMult(lN)
  val add = new BigAdd(2 * lN)
  val sub = new BigAdd(lN + 1)

  val innerCounter = Counter(lN * 2)

  val exponentCounter = Counter(lN)
  val exponentEnd = exponentCounter.valueNext === (parameterRegs.expononetLength - 1)

  // datapath
  val multOp0 = UInt(lN bits)
  val multOp1 = UInt(lN bits)
  val prod = UInt(2 * lN bits)
  def prodHigh = prod(2 * lN - 1 downto lN) // caution: val would lead to problems
  def prodLow = prod(lN - 1 downto 0)

  mult.input(0) := multOp0
  mult.input(1) := multOp1
  prod := mult.output
  prod.simPublic()

  sub.input(0) := add.output(2 * lN downto lN) // mid = (t + U * N) / Rho
  sub.input(1) := parameterRegs.N.resized // padding lN to lN + 1 bits with a leading zero

  multOp0 := U(0, lN bits)
  multOp1 := U(0, lN bits)
  add.input(0) := U(0)
  add.input(1) := U(0)

  val montRedcRet = add.output >> lN

  // << 1 leads to on bit more, resize would take lower(right) bits
  def exponentMove() = parameterRegs.expononet := (parameterRegs.expononet << 1).resized

  // template of montRedc subprocedure
  def montRedOnData(regForModedInput: UInt,
                    regForFullInput: UInt,
                    regsForOut: Vec[UInt], init: Int) = {
    when(innerCounter.value === U(init)) { // second cycle, first mult of montRedc
      multOp0 := regForModedInput // t mod Rho
      multOp1 := parameterRegs.omega
    }.elsewhen(innerCounter.value === U(init + 1)) {
      multOp0 := prodLow // U
      multOp1 := parameterRegs.N // N

      add.input(0) := regForFullInput.resized // t, may not be full width
      add.input(1) := prod // U * N
      val ret = Mux(sub.output >= 0,
        sub.output(lN - 1 downto 0),
        sub.input(0)(lN - 1 downto 0))
      regsForOut.foreach(_ := ret)
      // FIXME: add the reduction circuit
    }
  }

  def montMulOnData(input0: UInt, input1: UInt) = {
    when(innerCounter.value === U(0)) { // first cycle, square
      multOp0 := input0 // aMont ^ 2n / aMont ^ n
      multOp1 := input1 // aMont / aMont ^ n
      onGoingDataRegs := prod // aMont ^ (2n+1) / AMont ^ (2n)
    }
    montRedOnData(onGoingLow, onGoingDataRegs, Vec(onGoingLow), 1)
  }

  val fsm = new StateMachine {

    // state declarations
    val INIT = new StateDelayFixed(1) with EntryPoint // FIXME: this lasts for 3 cycles
    val PRE = new StateDelayFixed(2)
    val DoSquareFor1 = new StateDelayFixed(3)
    val DoMultFor1 = new StateDelayFixed(3)
    val DoSquareFor0 = new StateDelayFixed(3)
    val POST = new StateDelayFixed(2)

    val allStates = Seq(INIT, PRE, DoSquareFor1, DoMultFor1, DoSquareFor0, POST)
    allStates.foreach(_.whenIsActive(innerCounter.increment()))
    allStates.foreach(_.whenCompleted(innerCounter.clear()))

    // state transitions and counter maintenances
    INIT.whenCompleted(goto(PRE))
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

    // TODO: remove these later
    DoSquareFor0.cache.value.simPublic()

    val stateFlags = states.map(isActive(_))
    stateFlags.foreach(_.simPublic())

    val isDoSquareFor0 = isActive(DoSquareFor0)
    val isPRE = isActive(PRE)
    val isPOST = isActive(POST)

    isDoSquareFor0.simPublic()
    isPRE.simPublic()
    isPOST.simPublic()

    // state workload on datapath
    // TODO: merge INIT workload with PRE ?
    INIT.whenIsActive(parameterRegs := input) // data initialization
    PRE.whenIsActive(montRedOnData(parameterRegs.value, parameterRegs.value, Vec(initDataRegs, onGoingLow), 0))
    DoSquareFor1.whenIsActive(montMulOnData(onGoingLow, onGoingLow))
    DoMultFor1.whenIsActive(montMulOnData(onGoingLow, initDataRegs))
    DoSquareFor0.whenIsActive(montMulOnData(onGoingLow, onGoingLow))
    POST.whenIsActive(montRedOnData(onGoingLow, onGoingLow, Vec(output), 0))
  }

}

object Experiment {
  def main(args: Array[String]): Unit = {

    // DSP slice - inner pipeline
    GenRTL(new MontExp(512))
    val ref = new RSARef(512)
    val algo = new RSAAlgo(512)
    // get data from ref and algo for testing
    val exponent = ref.getPrivateValue
    val exponentLength = ref.getPrivateValue.toString(2).size
    val modulus = BigInt(ref.getModulus)
    val inputValue = (BigInt(ref.getPrivateValue) - DSPRand.nextInt(10000))
    val valueAfterPre = algo.montRed(inputValue, modulus)
    val omega = algo.getOmega(modulus)

    val paddedExponent = BigInt(exponent.toString(2).padTo(512, '0'), 2)

    def padLeft(value: BigInt) = BigInt(value.toString(2).padToLeft(512, '0'), 2)

    val paddedInputValue = padLeft(inputValue)
    val paddedValueAfterPre = padLeft(valueAfterPre)
    val paddedOmega = padLeft(omega)
    println(s"a = ${paddedInputValue.toString(16)}")
    println(s"aMont = ${paddedValueAfterPre.toString(16)}")
    println(s"paddedOmega = ${paddedOmega.toString(16)}")
    println(s"paddedExponent = ${paddedExponent.toString(16)}")

    SimConfig.withWave.compile(new MontExp(512)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)

      dut.input.expononet #= paddedExponent
      println(s"first 16 bits of the exponent ${paddedExponent.toString(2).take(16).mkString("")}")
      dut.input.expononetLength #= exponentLength
      println(exponentLength)

      dut.input.N #= modulus
      dut.input.omega #= omega
      dut.input.RhoSquare #= algo.getRhoSquare(modulus)

      dut.input.value #= inputValue
      println(s"cycles should be ${exponent.toString(2).tail.map(_.asDigit + 1).sum * 3 + 2}")

      (0 until 3000).foreach { _ =>
        //        println(s"on: ${dut.fsm.isDoSquareFor0.toBoolean}, cache: ${dut.fsm.DoSquareFor0.cache.value.toInt}")
        dut.clockDomain.waitSampling()
        if (dut.fsm.isPRE.toBoolean) {
          println(s"PRE at $simTime")
          println(s"prod ${padLeft(dut.prod.toBigInt).toString(16)}")
        }
        if (dut.fsm.isPOST.toBoolean) println(s"POST at $simTime")
      }
    }
  }
}