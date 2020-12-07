// design :
// 功能 : 交织
// 协议 : Stream(握手) + Fragment
// 综合要求 : 双口BRAM
// 性能要求 : 300MHz

package FTN

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random
import spinal.lib.fsm._

class Interleave(wordWidth: Int, sizeRd: Int, sizeWr: Int) extends Component {

  //  def prediction() = { // todo : 根据参数预测BRAM用量
  //    println("BRAM consumption = ")
  //  }
  def typeWord = UInt(wordWidth bits)

  val depth = sizeRd * sizeWr

  val io = new Bundle {
    val input = slave Stream (Fragment(typeWord))
    val output = master Stream (Fragment(typeWord))
  }

  val ping = Mem(typeWord, depth) // mem0 // design : 实现ping-pong buffer
  val pong = Mem(typeWord, depth) // mem1

  val ptrWr = Reg(UInt(1 bits)) init (U(0)) // design : 重置后,读写指针都指向ping
  val ptrRd = Reg(UInt(1 bits)) init (U(0))

  // -----读写逻辑-----//
  val counterWr = Counter(0 until depth, io.input.fire)
  val doneWr = counterWr.willOverflowIfInc
  val countWr = counterWr.value

  val addressWr = UInt(log2Up(depth) bits)
  addressWr := countWr
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

    EMPTY
      .whenIsActive {
        when(doneWr)(goto(HALF))
      }

    HALF
      .whenIsActive {
        when(doneWr && doneRd)(goto(HALF))
          .elsewhen(doneWr)(goto(FULL))
          .elsewhen(doneRd)(goto(EMPTY))
      }

    FULL
      .whenIsActive {
        when(doneRd)(goto(HALF))
      }

    io.input.ready := isActive(EMPTY) || isActive(HALF)
    io.output.valid := isActive(FULL) || isActive(HALF)
  }

  io.output.payload.last := doneRdDelay

  noIoPrefix()
}

object Interleave {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new Interleave(8, 32, 64))
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
//          // 模式1,读写通畅
//          for (i <- 0 until 100) {
//            dut.io.input.valid #= true
//            dut.io.output.ready #= true
//            dut.io.input.payload.fragment #= i % 32
//            sleep(period)
//          }
          // 模式2,只写不读
          for (i <- 0 until 40) {
            dut.io.input.valid #= true
            dut.io.output.ready #= false
            dut.io.input.payload.fragment #= i % 32
            sleep(period)
          }
          // 模式3,只读不写
          for (i <- 0 until 40) {
            dut.io.input.valid #= false
            dut.io.output.ready #= true
            dut.io.input.payload.fragment #= i % 32
            sleep(period)
          }
          // 模式4,交替读写
          for (i <- 0 until 100) {
            dut.io.input.valid #= (if (i / 10 % 2 == 0) true else false)
            dut.io.output.ready #= (if (i / 10 % 2 == 1) true else false)
            dut.io.input.payload.fragment #= i % 32
            sleep(period)
          }
          simSuccess()
        }
      }
  }
}
