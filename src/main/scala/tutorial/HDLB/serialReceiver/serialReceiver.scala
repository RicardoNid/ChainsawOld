package tutorial.HDLB.serialReceiver

import tutorial.HDLB.StateMachineNoBoot
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// HDLBits URL = https://hdlbits.01xz.net/wiki/Fsm_serial
class serialReceiver extends Component { // todo : 没搞懂题目的逻辑

  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val in_ = in Bool
    val done = out Bool
  }

  val CD = ClockDomain(
    clock = io.clk,
    config = new ClockDomainConfig(resetKind = BOOT, resetActiveLevel = HIGH))

  val mainCD = new ClockingArea(CD) {
    val fsm = new StateMachineNoBoot {
      val IDLE = StateEntryPoint()
      val DATA = new StateDelay(8).whenCompleted(goto(BRANCH))
      val BRANCH = State()
      val STOP = State()
      val NONE = State()

      IDLE.whenIsActive(when(!io.in_)(goto(DATA)))
      BRANCH.whenIsActive {
        when(io.in_)(goto(STOP))
          .otherwise(goto(NONE))
      }
      STOP.whenIsActive {
        when(io.in_)(goto(IDLE))
          .otherwise(goto(DATA))
      }
      NONE.whenIsActive(goto(IDLE))

      io.done := isActive(STOP)
    }
  }

  noIoPrefix()
}

object serialReceiver {
  def main(args: Array[String]): Unit = {
    SpinalConfig(
      netlistFileName = "serialReceiver.sv",
      targetDirectory = "output/HDLBits")
      .generateSystemVerilog(new serialReceiver().setDefinitionName("top_module"))
  }
}
