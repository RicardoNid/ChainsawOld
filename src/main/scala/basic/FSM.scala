package basic

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._
import scala.util.Random

class FSM extends Component {
  val io = new Bundle {
    val result = out Bool
  }

  val fsm = new StateMachine {
    val counter = Reg(UInt(8 bits)) init (0)
    io.result := False

    val stateA: State = new State with EntryPoint {
      whenIsActive(goto(stateB))
    }
    val stateB = new State {
      onEntry{counter := 0}
      whenIsActive {
        counter := counter + 1
        when(counter === 4) {
          goto(stateC)
        }
      }
      onExit(io.result := True)
    }
    val stateC = new State {
      whenIsActive((goto(stateA)))
    }
  }
}

object FSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new FSM)
  }
}

object testFSM {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FSM).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.risingEdge()
          while (true) {
            dut.clockDomain.clockToggle()
            sleep(1)
          }
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}
