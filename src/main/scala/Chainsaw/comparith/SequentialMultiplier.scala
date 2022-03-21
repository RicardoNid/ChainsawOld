package Chainsaw.comparith

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import scala.annotation.tailrec

class SequentialMultiplier(k: Int, signed: Boolean = false) extends Component {

  val io = new Bundle {
    val start = in Bool ()
    val a, x  = in Bits (k bits)
    val p     = out Bits (2 * k bits)
    val valid = out Bool ()
  }

  val a = io.a.asUInt
  val x = io.x.asUInt

  val count = Counter(k)
  io.valid := RegNext(count.willOverflow, init = False)

  val partialReg  = Reg(UInt(2 * k bits))
  val aReg        = RegNextWhen(a, io.start)
  val xLast       = partialReg.lsb
  val partialHigh = partialReg(2 * k - 1 downto k)
  val partialLow  = partialReg(k - 1 downto 0)
  io.p := partialReg.asBits

  if (!signed) {
    val fsm = new StateMachine {
      val IDLE = StateEntryPoint()
      val RUN  = State()
      IDLE.whenIsActive(when(io.start)(goto(RUN)))
      RUN.whenIsActive(when(count.willOverflow)(goto(IDLE)))

      RUN.whenIsActive {
        count.increment()
        when(count.value === U(0))(partialReg := (Mux(io.x.lsb, a, U(0)) @@ x) |>> 1)
          .otherwise(partialReg := (Mux(xLast, partialHigh +^ aReg, partialHigh) @@ partialLow) >> 1)
      }
    }
  } else {
    val fsm = new StateMachine {
      val IDLE = StateEntryPoint()
      val RUN  = State()
      IDLE.whenIsActive(when(io.start)(goto(RUN)))
      RUN.whenIsActive(when(count.willOverflow)(goto(IDLE)))

      RUN.whenIsActive {
        count.increment()

        //        val currentBit = Mux(count.value === U(0), x.lsb, xLast)
        //        val anotherOperand = Mux(currentBit, Mux(count.value === U(k - 1), ~aReg, aReg), U(0))
        //        val carry = Mux(count.value === U(k - 1) && currentBit, U(1, 1 bits), U(0, 1 bits))
        //        when(count.value === U(0))(partialReg := (Mux(x.lsb, a, U(0)) @@ x) |>> 1)
        //          .otherwise(partialReg := ((partialHigh +^ anotherOperand + carry) @@ partialLow) >> 1)

        // code below lead to two adders which won't be merged ?
        // TODO: try opt_design and see whether this can be merged by the synthesizer
        // on the other hand, the second version has longer critical path
        when(count.value === U(0))(partialReg := (Mux(x.lsb, a, U(0)) @@ x) |>> 1)
          .elsewhen(count.value === U(k - 1))(partialReg := (Mux(xLast, partialHigh +^ (~aReg) + U(1, 1 bits), partialHigh) @@ partialLow) >> 1)
          .otherwise(partialReg := (Mux(xLast, partialHigh +^ aReg, partialHigh) @@ partialLow) >> 1)
      }
    }
  }
}

/** @see
  *   Computer Arithmetic, page 180
  */
object SequentialMultiplier { // right-shift version

  def software(a: BigInt, x: BigInt, signed: Boolean = false): BigInt = {
    val k = a.bitLength max x.bitLength
    @tailrec
    def recursion(j: Int, partial: BigInt): BigInt = {
      if (j == k) partial
      else {
        val newPartial =
          if (signed && j == k - 1) (if (x.testBit(j)) partial - (a << k) else partial) >> 1
          else (if (x.testBit(j)) partial + (a << k) else partial)                      >> 1
        recursion(j + 1, newPartial)
      }
    }
    recursion(0, 0)
  }

  def main(args: Array[String]): Unit = {
    println(software(5, 3))
    println(software(5, -3, signed = true))
    def sim(signed: Boolean) = {
      SimConfig.withWave.compile(new SequentialMultiplier(4, signed)).doSimUntilVoid { dut =>
        import dut._
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()
        io.a     #= 5
        io.x     #= BigInt("1101", 2) // 13 / -3
        io.start #= true
        while (true) {
          if (io.valid.toBoolean) {
            assert(io.p.toInt == (if (signed) 128 - 15 else 65)) // 128 - 15 is -15 interpreted as unsigned
            simSuccess()
          }
          clockDomain.waitSampling()
        }
      }
    }
    //    sim(false)
    //    sim(true)
    //    VivadoSynth(new SequentialMultiplier(16))
    //    VivadoSynth(new SequentialMultiplier(16, true))
  }
}
