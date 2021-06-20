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
//  import spinal.core.sim._
//
//  val io = new Bundle {
//    val start = in Bool()
//    val mode = in Bits()
//
//    val xiIn = in UInt (1 bits)
//    val rSquareWordIn = in UInt (w bits) // Y for the first MontMul
//    val MWordIn = in UInt(w bits)
//
//    // assume no length provided, find out the first valid
//
//    // TODO: transfer exponent in a more realistic manner
//    val ExponentWordIn = in UInt(lMs.max bits)
//    val ExponentLength = in UInt(log2Up(lMs.max + 1) bits)
//
//    val dataOut = out UInt (w bits)
//    val valid = out Bool()
//  }
//
//  val mult = MontMulSystolic(config)
//  mult.io.start := io.start
//  mult.io.mode := io.mode
//  io.dataOut := mult.io.dataOut
//
//
//  val xMontWordsRAM = Mem(UInt(w bits), lMs.max / w)
//  val MWordsRAM = Mem(UInt(w bits), lMs.max / w)
////  val previousResultFIFO = FIFO(UInt(w bits), )
//
//  val fsm = new StateMachine{
//    val IDLE  = StateEntryPoint()
//    val INIT = new State() // TODO: try to avoid this
//    val PRE, MID, POST = new State()
//
//    IDLE.whenIsActive{
//      when(io.start)(goto(PRE))
//    }
//
//    PRE.whenIsActive{
//      mult.io.xiIn
//      mult.io.YWordIn :=
//    }
//
//
//  }
//
//}
