//package Chainsaw.Crypto.RSA
//
//import spinal.core._
//import spinal.core.sim._
//import spinal.lib._
//import spinal.sim._
//import spinal.lib.fsm._
//
//import Chainsaw._
//import Chainsaw.Real
//
//case class MontExpSystolic(config: MontConfig) extends Component {
//
//  import config._
//
//  require(isPow2(w))
//  require(lMs.forall(lM => isPow2(lM / w))) // not valid for 3072
//
//  val io = new Bundle {
//    val start = in Bool()
//    val mode = in Bits()
//
//    val XWordIns = in Vec(UInt(w bits), parallelFactor)
//    val rSquareWordIn = in UInt (w bits) // Y for the first MontMul
//    val MWordIn = in UInt (w bits)
//    val ExponentWordIn = in UInt (lMs.max bits)
//    val ExponentLengthIn = in UInt (log2Up(lMs.max + 1) bits)
//
//    val dataOut = out UInt (w bits)
//    val valid = out Bool()
//  }
//
//  // memories
//  // TODO: use less than e?
//  val rSquareWordRAM, MWordRAM, ExponentWordRAM = Mem(UInt(w bits), es.max)
//  // TODO: reduce redundancy
//  val XWordRAMs, partialProductWordRAMs = Seq.fill(parallelFactor)(Mem(UInt(w bits), es.max))
//  val ExponentLengthReg = Reg(HardType(io.ExponentLengthIn))
//  val mult = MontMulSystolic(config)
//
//  // TODO: merge this with the inner modeReg
//  val modeReg = Reg(HardType(io.mode))
//  when(io.start)(modeReg := io.mode)
//
//  // TODO: merge this with the inner eCounter
//  val eCounter = Counter(es.max)
//  val eCounterWillOverflows = es.map(e => eCounter.value === U(e - 1) && eCounter.willIncrement)
//  val currentECounterOverflow = MuxOH(modeReg, eCounterWillOverflows)
//  when(currentECounterOverflow)(eCounter.clear())
//
//  val roundCounter = Counter(rounds.max, inc = currentECounterOverflow)
//  val roundCounterWillOverflows = rounds.map(round => roundCounter.value === U(round - 1) && roundCounter.willIncrement)
//  val currentRoundCounterOverflow = MuxOH(modeReg, roundCounterWillOverflows)
//  when(currentRoundCounterOverflow)(roundCounter.clear())
//
//  // maintain the location of X
//  val xCounter = Counter(p * rounds.max)
//  println(s"X lengths = ${parallelPs.zip(rounds).map { case (p, r) => p * r }.mkString(" ")}")
//  val xCounterWillOverflows =
//    parallelPs.zip(rounds).map { case (p, r) => p * r }
//      .map(end => xCounter.value === U(end - 1) && xCounter.willIncrement)
//  val currentXCounterOverflow = MuxOH(modeReg, xCounterWillOverflows)
//  when(currentXCounterOverflow)(xCounter.clear())
//  val xRAMIdOffset = xCounter.value
//  val xAddr = xCounter.value >> log2Up(w)
//  val xBitAddr = xCounter.value(log2Up(w) - 1 downto 0)
//
//  val fsm = new StateMachine {
//    val IDLE = StateEntryPoint()
//    val INIT = new State() // TODO: try to avoid this
//    val PRE, MID, POST = new State()
//
//    IDLE.whenIsActive {
//      when(io.start)(goto(INIT))
//    }
//
//    INIT.whenIsActive { // lasts for e cycles
//      XWordRAMs.zip(io.XWordIns) foreach { case (ram, x) =>
//        ram(eCounter.value) := x
//      }
//      rSquareWordRAM(eCounter.value) := io.rSquareWordIn
//      MWordRAM(eCounter.value) := io.MWordIn
//      ExponentWordRAM(eCounter.value) := io.ExponentWordIn
//      ExponentLengthReg := io.ExponentLengthIn
//      when(currentECounterOverflow) {
//        goto(PRE)
//        mult.io.start := True
//      }
//    }
//
//    PRE.whenIsActive { // lasts for e * r cycles
//      mult.io.YWordIn := rSquareWordRAM(eCounter.value)
//      mult.io.MWordIn := MWordRAM(eCounter.value)
//      mult.io.mode := modeReg
//      mult.io.xiIn :=
//    }
//  }
//}
//
