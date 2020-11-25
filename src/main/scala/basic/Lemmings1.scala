package basic

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random
import spinal.lib.fsm._

class Lemming1 extends Component {
  val io = new Bundle {
    val clk = in Bool
    val areset = in Bool
    val bump_left = in Bool
    val bump_right = in Bool
    val walk_left = out Bool
    val walk_right = out Bool
  }

  val clockConfig = ClockDomainConfig(resetKind = ASYNC)
  new ClockingArea(new ClockDomain(clock = io.clk, reset = io.areset, config = clockConfig)) {

    object stateType extends SpinalEnum {
      val walkLeftS, walkRightS = newElement()
    }

    import stateType._

    val nextState = stateType()
    val state = RegNext(nextState) init (walkLeftS)

    nextState := state
    when(state === walkLeftS && io.bump_left) {
      nextState := walkRightS
    }
    when(state === walkRightS && io.bump_right) {
      nextState := walkLeftS
    }

    io.walk_left := (state === walkLeftS)
    io.walk_right := (state === walkRightS)

    // design : fsm库的语法和逻辑我很喜欢,但是由于自带一个不能跳过(至少我不知道怎么做)的bootState,反而难以处理这个HDLBits上这些简单的状态机
    // design : 这也说明一个问题,当高级特性的设计与你预期不符时,你始终可以通过写verilog-style的代码来回避
    //
    //    val fsm = new StateMachine {
    //
    //      val walkLeftS = new State with EntryPoint
    //      val walkRightS = new State
    //
    //      walkLeftS.whenIsActive {
    //        when(io.bump_left) {
    //          goto(walkRightS)
    //        }
    //      }
    //
    //      walkRightS.whenIsActive {
    //        when(io.bump_right) {
    //          goto(walkLeftS)
    //        }
    //      }
    //
    //      io.walk_left := isActive(walkLeftS)
    //      io.walk_right := isActive(walkRightS)
    //    }

    noIoPrefix()
  }
}

object Lemming1 {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new Lemming1().setDefinitionName("top_module"))
  }
}

object testLemming1 {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Lemming1).
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
