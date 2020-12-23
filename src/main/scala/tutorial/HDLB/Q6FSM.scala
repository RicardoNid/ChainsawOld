package tutorial.HDLB

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class Q6FSM extends Component {
  val io = new Bundle {
    val clk = in Bool
    val reset = in Bool

    val w = in Bool
    val z = out Bool
  }

  val clockConfig = ClockDomainConfig(resetKind = SYNC)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.reset, config = clockConfig)) {
    val fsm = new StateMachine {
      val A = StateEntryPoint()
      val B = State()
      val C = State()
      val D = State()
      val E = State()
      val F = State()

      io.z := False
      A.whenIsActive {
        (when(!io.w)(goto(B)).otherwise(goto(A)))
        io.z := False
      }
      B.whenIsActive {
        (when(!io.w)(goto(C)).otherwise(goto(D)))
        io.z := False
      }
      C.whenIsActive {
        (when(!io.w)(goto(E)).otherwise(goto(D)))
        io.z := False
      }
      D.whenIsActive {
        (when(!io.w)(goto(F)).otherwise(goto(A)))
        io.z := False
      }
      E.whenIsActive {
        (when(!io.w)(goto(E)).otherwise(goto(D)))
        io.z := True
      }
      F.whenIsActive {
        (when(!io.w)(goto(C)).otherwise(goto(D)))
        io.z := True
      }
    }

  }
  noIoPrefix()
}

object Q6FSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Q6FSM().setDefinitionName("top_module"))
  }
}
