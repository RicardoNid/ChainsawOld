package tutorial.basic

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._
import scala.util.Random

class FSMTemplate extends Component {
  val io = new Bundle {
    val clk = in Bool
    val rstn = in Bool
    val input = in Vec(UInt(4 bits), 4)
    val output = out Bits (2 bits)
  }

  noIoPrefix()

  // 设置时钟域
  val CDConfig = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW)
  val mainCD = ClockDomain(clock = io.clk, reset = io.rstn, config = CDConfig)

  val mainArea = new ClockingArea(mainCD) {
    // 声明寄存器
    val regRecord = Reg(UInt(8 bits)) init (U(0))

    // 状态机, 定义核心行文
    val mainFSM = new StateMachine {
      val S0 = new State with EntryPoint
      val S1 = new State
      val S2 = new State

      S0
        .whenIsActive(goto(S1))

      S1
        .whenIsActive(goto(S2))
        .onExit(regRecord := regRecord + 1)

      S2
        .whenIsActive(goto(S0))
    }


    // 定义reset行为
    when(!io.rstn) {
      regRecord := U(0)
    }
  }
}

object FSMTemplate {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new FSMTemplate)
  }
}

object testFSMTemplate {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FSMTemplate).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.mainCD.forkStimulus(period = period)
        }
        val mainThread = fork {
          sleep(period * 100)
          dut.io.rstn #= false
          sleep(period)
          dut.io.rstn #= true
          sleep(100)
          simSuccess()
        }
      }
  }
}
