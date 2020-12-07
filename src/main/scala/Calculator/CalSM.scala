package Calculator

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random
import spinal.lib.fsm._

class CalSM extends Component {
  val io = new Bundle {
    // val rst = in Bool 采用隐式reset,不声明
    // 按键输入
    val buttonLeft = in Bool
    val buttonRight = in Bool
    val buttonUp = in Bool
    val buttonDown = in Bool
    val buttonConfirm = in Bool
    // 提供给显示模块的编码
    val displaySymbol = out UInt (10 bits)
    val displayFlash = out Bits (2 bits)
  }

  val ADD = U(1000)
  val SUB = U(1001)
  val MULT = U(1002)

  val ERR = U(1023)

  val flashReg = Reg(Bits(2 bits)) init (B"00")
  val displayReg = Reg(UInt(10 bits)) init (U(0))
  val accReg = Reg(UInt(10 bits)) init (U(0))
  val opReg = Reg(UInt(10 bits)) init (U(1000))
  val tempReg = Reg(UInt(20 bits)) init (U(0))

  val errTimeout = Timeout(500 ms)

  // 主状态机
  val mainSM = new StateMachine {
    val OPRAND1 = new State with EntryPoint
    val OP = new State
    val OPRAND2 = new State
    val ERROR = new State

    OPRAND1
      .onEntry {
        displayReg := tempReg.resized
        accReg := tempReg.resized
      }
      .whenIsActive {
        when(io.buttonConfirm) {
          accReg := displayReg
          goto(OP)
        }
      }

    OP
      .onEntry(displayReg := ADD)
      .whenIsActive {
        when(io.buttonConfirm) {
          opReg := displayReg
          goto(OPRAND2)
        }
      }

    OPRAND2
      .onEntry(displayReg := U(1))
      .whenIsActive {
        when(io.buttonConfirm) {
          switch(opReg) {
            is(ADD)(tempReg := (accReg +^ displayReg).resized)
            is(SUB)(tempReg := (accReg -^ displayReg).resized)
            is(MULT)(tempReg := accReg * displayReg)
          }
          when(tempReg > U(999))(goto(ERROR))
            .otherwise(goto(OPRAND1))
        }
      }

    ERROR
      .onEntry {
        errTimeout.clear()
      }
      .whenIsActive {
        displayReg := ERR
        when(errTimeout === True)(goto(OPRAND1))
      }
  }

  // 数字/字符输入状态机

  io.displaySymbol := displayReg
  io.displayFlash := flashReg
}

object CalSM {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new CalSM)
  }
}

object testCalSM {

  val period = 2

  def main(args: Array[String]): Unit = {
    val config = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(100 MHz))
    SimConfig.withWave.withConfig(config).compile(new CalSM).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          sleep(1)
          for (i <- 0 until 1000) {
            dut.io.buttonConfirm #= (if (i % 5 == 0) true else false)
            sleep(period)
          }
          simSuccess()
        }
      }
  }
}
