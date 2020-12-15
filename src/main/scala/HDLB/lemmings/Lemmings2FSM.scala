package HDLB.lemmings

import HDLB.StateMachineNoBoot
import spinal.core._
import spinal.lib.fsm._

class Lemmings2FSM extends Component {
  val io = new Bundle {
    val clk = in Bool
    val areset = in Bool
    val bump_left = in Bool
    val bump_right = in Bool
    val ground = in Bool
    val walk_left = out Bool
    val walk_right = out Bool
    val aaah = out Bool
  }

  val clockConfig = ClockDomainConfig(resetKind = ASYNC)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.areset, config = clockConfig)) {


    val fsm = new StateMachineNoBoot {
      val WALK = StateEntryPoint()
      val FALL = State()

      WALK
        .whenIsActive(when(!io.ground)(forceGoto(FALL)))

      FALL
        .whenIsActive(when(io.ground)(goto(WALK)))

      val directionLeft = RegInit(True)
      directionLeft.setWhen(!directionLeft && io.ground && isActive(WALK) && io.bump_right)
      directionLeft.clearWhen(directionLeft && io.ground && isActive(WALK) && io.bump_left)

      io.walk_left := isActive(WALK) && directionLeft
      io.walk_right := isActive(WALK) && !directionLeft
      io.aaah := isActive(FALL)
    }
  }
  noIoPrefix()
}

object Lemmings2FSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Lemmings2FSM().setDefinitionName("top_module"))
  }
}


