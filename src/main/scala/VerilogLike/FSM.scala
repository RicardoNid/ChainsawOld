package VerilogLike

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.fsm.{State, StateMachine}

import scala.util.Random

class FSM extends Component {
    val io = new Bundle {
    val output = out Bool
  }

  val fsm = new StateMachine { // design : 取消boot的特殊行为,将boot作为"第一个",而非"第零个"状态使用
    val S0 = stateBoot
    setEntry(S0)
    S0.setName("S0")
    val S1 = State()
    disableAutoStart()

    val timeout = Timeout(2)

    S0.onEntry(timeout.clear())
      .whenIsActive(when(timeout)(goto(S1)))
    S1.onEntry(timeout.clear())
      .whenIsActive(when(timeout)(goto(S0)))

    io.output := isActive(S1)
  }
}

object FSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new FSM)
  }
}

object testFSM {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FSM).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          sleep(100 * period)
          simSuccess()
        }
      }
  }
}
