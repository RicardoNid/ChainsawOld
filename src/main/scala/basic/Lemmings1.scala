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
