package HDLB.lemmings

import HDLB.StateMachineNoBoot
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Lemmings4
class Lemmings4 extends Component { // design : one-shoot success on this!

  val io = new Bundle {
    val clk = in Bool
    val areset = in Bool

    val bump_left = in Bool
    val bump_right = in Bool
    val ground = in Bool
    val dig = in Bool

    val walk_left = out Bool
    val walk_right = out Bool
    val aaah = out Bool
    val digging = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    reset = io.areset,
    config = new ClockDomainConfig(resetKind = ASYNC, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val fsm = new StateMachineNoBoot {
      val WALK = StateEntryPoint()
      val DIG = State()
      val FALL = State()
      val DEAD = State()

      val fallTimeout = Timeout(20)

      // state transition logic
      WALK.whenIsActive {
        when(!io.ground)(goto(FALL))
          .elsewhen(io.dig)(goto(DIG))
      }

      DIG.whenIsActive(when(!io.ground)(goto(FALL)))

      FALL // design : "状态计时器"的典型用法
        .onEntry(fallTimeout.clear())
        .whenIsActive {
          when(io.ground) {
            when(fallTimeout)(goto(DEAD))
              .otherwise(goto(WALK))
          }
        }

      // direction logic
      val directionLeft = RegInit(True)
      val listen = io.ground && isActive(WALK) && !io.dig // direction can change when "listening"
      directionLeft.setWhen(listen && !directionLeft && io.bump_right)
      directionLeft.clearWhen(listen && directionLeft && io.bump_left)

      // output logic
      io.walk_left := isActive(WALK) && directionLeft
      io.walk_right := isActive(WALK) && !directionLeft
      io.aaah := isActive(FALL)
      io.digging := isActive(DIG)
    }
  }

  noIoPrefix()
}

object Lemmings4 {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "Lemmings4.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new Lemmings4().setDefinitionName("top_module"))
  }
}
