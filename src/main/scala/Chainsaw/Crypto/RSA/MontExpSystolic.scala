package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class MontExpSystolic(lMs: Seq[Int], w: Int, p: Int) extends Component {

  val io = new Bundle {
    val start = in Bool()
    val mode = in Bits()
    val rSquareWordIn = in UInt (w bits) // Y for the first MontMul
    val XWordIn = in UInt (w bits)
    val MWordIn = in UInt(w bits)
    // assume no length provided, find out the first valid
    val ExponentWordIn = in UInt(w bits)
    val dataOut = out UInt (w bits)
  }

  val mult = MontMulSystolic(lMs, w, p = p)
  mult.io.start := io.start

  val xMontWordsRAM = Mem(UInt(w bits), lMs.max / w)
  val MWordsRAM = Mem(UInt(w bits), lMs.max / w)

  val fsm = new StateMachine{
    val IDLE  = StateEntryPoint()
    val INIT = new State() // TODO: try to avoid this
    val PRE, MID, POST = new State()

  }

}
