package HDLB.lemmings

import HDLB.StateMachineNoBoot
import spinal.core._
import spinal.lib.fsm._

class Lemmings3FSM extends Component {
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

  val clockConfig = ClockDomainConfig(resetKind = ASYNC)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.areset, config = clockConfig)) {

    val fsm = new StateMachineNoBoot {
      val WALK = StateEntryPoint()
      val FALL = State()
      val DIG = State()

      WALK
        .whenIsActive {
          when(!io.ground)(goto(FALL))
            .elsewhen(io.dig)(goto(DIG))
        }

      FALL.whenIsActive(when(io.ground)(goto(WALK)))

      DIG.whenIsActive(when(!io.ground)(goto(FALL)))

      val directionLeft = RegInit(True)
      val listen = io.ground && isActive(WALK) && !io.dig
      directionLeft.setWhen(listen && !directionLeft && io.bump_right)
      directionLeft.clearWhen(listen && directionLeft && io.bump_left)

      io.walk_left := isActive(WALK) && directionLeft
      io.walk_right := isActive(WALK) && !directionLeft
      io.aaah := isActive(FALL)
      io.digging := isActive(DIG)
    }
  }
  noIoPrefix()
}

object Lemmings3FSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Lemmings3FSM().setDefinitionName("top_module"))
  }
}
