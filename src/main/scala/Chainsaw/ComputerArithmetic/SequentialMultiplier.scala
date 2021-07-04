package Chainsaw.ComputerArithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class SequentialMultiplier(k: Int, careful: Boolean = true) extends Component {

  val io = new Bundle {
    val start = in Bool()
    val a, x = in UInt (k bits)
    val p = out UInt (2 * k bits)
    val valid = out Bool()
  }

  val count = Counter(k)
  val xReg = RegNextWhen(io.x, io.start)
  //  val currentBit = if (careful) xReg.lsb else xReg(count.value) // raw implementation(software-like), which would be very bad in performance
  val currentBit = xReg.lsb // raw implementation(software-like), which would be very bad in performance

  io.valid := RegNext(count.willOverflow, init = False)

  val partial = Reg(UInt(2 * k bits))
  val partialHigh = partial.splitAt(k)._1.asUInt
  val partialLow = partial.splitAt(k)._2.asUInt
  io.p := partial

  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val RUN = State()
    IDLE.whenIsActive(when(io.start)(goto(RUN)))
    RUN.whenIsActive(when(count.willOverflow)(goto(IDLE)))

    RUN.whenIsActive {
      count.increment()
      // careless version
      if (!careful) {
        when(count.value === U(0))(partial := (Mux(currentBit, io.a << k, U(0)) >> 1).resized) // the msb is 0 at the first place
          .otherwise(partial := Mux(currentBit, partial +^ (io.a << k), partial) >> 1)
        xReg := xReg |>> 1
      }
      else {
        when(count.value === U(0))(partial := (Mux(currentBit, io.a << k, U(0)) >> 1).resized) // the msb is 0 at the first place, one bit padded
          //          .otherwise(partial := Mux(currentBit, (partialHigh +^ io.a) @@ partialLow, partial) >> 1)
          .otherwise(partial := (Mux(currentBit, partialHigh +^ io.a, partialHigh) @@ partialLow) >> 1)
        xReg := xReg |>> 1
      }
    }
  }
}

/**
 * @see Computer Arithmetic, page 180
 */
object SequentialMultiplier { // right-shift version

  def software(a: BigInt, x: BigInt): BigInt = {
    val k = a.bitLength max x.bitLength
    def recursion(j: Int, partial: BigInt): BigInt = {
      if (j == k) partial
      else {
        val newPartial = (if (x.testBit(j)) partial + (a << k) else partial) >> 1
        recursion(j + 1, newPartial)
      }
    }
    recursion(0, 0)
  }

  def main(args: Array[String]): Unit = {
    println(software(3, 5))
    def sim(careful: Boolean) = {
      SimConfig.withWave.compile(new SequentialMultiplier(4, careful)).doSimUntilVoid { dut =>
        import dut._
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()
        io.a #= 3
        io.x #= 5
        io.start #= true
        while (true) {
          if (io.valid.toBoolean) {
            assert(io.p.toInt == 15)
            simSuccess()
          }
          clockDomain.waitSampling()
        }
      }
    }
    sim(true)
    sim(false)
    VivadoSynth(new SequentialMultiplier(16, true))
    VivadoSynth(new SequentialMultiplier(16, false))
  }
}
