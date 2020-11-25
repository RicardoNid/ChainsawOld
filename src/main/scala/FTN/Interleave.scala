// design :
// 功能 : 交织
// 协议 : Stream(握手)
// 综合要求 : 双口BRAM
// 性能要求 : 300MHz

package FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random
import spinal.lib.fsm._

class Interleave(wordWidth: Int, sizeRd: Int, sizeWr: Int) extends Component {

  def prediction() = { // todo : 根据参数预测BRAM用量
    println("BRAM consumption = ")
  }

  def typeWord = UInt(wordWidth bits)

  val depth = sizeRd * sizeWr

  val io = new Bundle {
    val input = slave Stream (typeWord)
    val output = master Stream (typeWord)
  }

  val ping = Mem(typeWord, depth) // mem0
  val pong = Mem(typeWord, depth) // mem1

  val ptrWr = Reg(UInt(1 bits)) init (U(0)) // design : 重置后,读写指针都指向ping
  val ptrRd = Reg(UInt(1 bits)) init (U(0))

  // -----读写逻辑-----//
  val counterWr = Counter(0 until depth, io.input.fire)
  val doneWr = counterWr.willOverflowIfInc
  val countWr = counterWr.value
  val addressWr = UInt(log2Up(depth) bits)
  addressWr := ((countWr % U(sizeWr)) * U(sizeRd) + countWr / U(sizeWr)).resized
  ping.write(addressWr, io.input.payload, io.input.fire && ptrWr === U(0)) // design : 对不同mem的选择不能通过when进行,因为mem的读写方法是描述连线,不是描述事件
  pong.write(addressWr, io.input.payload, io.input.fire && ptrWr === U(1))

  val counterRd = Counter(0 until depth, io.output.fire)
  val doneRd = counterRd.willOverflowIfInc
  val countRd = counterRd.value
  val addressRd = UInt(log2Up(depth) bits)
  addressRd := ((countRd % U(sizeRd)) * U(sizeWr) + countRd / U(sizeRd)).resized
  // fixme : 通过一些技巧可以消除除法和取模
  val pingOut = ping.readSync(addressRd, io.output.fire)
  val pongOut = pong.readSync(addressRd, io.output.fire)
  io.output.payload := Mux(ptrRd === U(0), pingOut, pongOut)

  // -----状态机部分-----//
  val doneRdDelay = Delay(doneRd, 1) init (False)
  when(doneWr)(ptrWr := ~ptrWr) // design : 单行内容可以使用()
  when(doneRdDelay)(ptrRd := ~ptrRd)

  val fsm = new StateMachine {
    val EMPTY = new State with EntryPoint
    val HALF = new State
    val FULL = new State

    io.input.ready := False
    io.output.valid := False

    EMPTY
      .whenIsActive {
        when(doneWr)(goto(HALF))
        io.input.ready := True
        io.output.valid := False
      }

    HALF
      .whenIsActive {
        when(doneWr)(goto(FULL))
          .elsewhen(doneRd)(goto(EMPTY))
        io.input.ready := True
        io.output.valid := True
      }

    FULL
      .whenIsActive {
        when(doneRd)(goto(HALF))
        io.input.ready := False
        io.output.valid := True
      }
  }

  noIoPrefix()
}

object Interleave {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new Interleave(8, 4, 8))
  }
}

object testInterleave {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new Interleave(8, 2, 4)).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          sleep(period * 17)
          sleep(period / 2)
          // 模式1,读写通畅
          for (i <- 0 until 40) {
            dut.io.input.valid #= true
            dut.io.output.ready #= true
            dut.io.input.payload #= i % 32
            sleep(period)
          }
          // 模式2,只写不读
          for (i <- 0 until 40) {
            dut.io.input.valid #= true
            dut.io.output.ready #= true
            dut.io.input.payload #= i % 32
            sleep(period)
          }
          simSuccess()
        }
      }
  }
}
