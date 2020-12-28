//package sysu.util
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.core.sim._
//import sysu.CNN.{LoopNestConv, convTransformer}
//
//import scala.util.Random
//
//// 初步的实现,额外约束条件包括
//// 1.loopNest各个维度的bound都是常数 -> todo 依赖其他var的bound,以及相应的动态计数器
//// 2.对各个计数器变量只进行% / +,没有*(step = 1) -> todo 实现step = n, 实现escape
//// 3.
//class ArithmeticSeqGen[T](loopNestConv: LoopNestConv, access:convTransformer[T]) extends Component {
//
//  val io = new Bundle {
//    val output = out UInt (10 bits)
//  }
//
//  Counter
//
//  val baseReg = Reg(UInt(7 bits)) init (0)
//
//  val count = Counter(end, True)
//  val modCount1 = Counter(mod1, True)
//  val modCount2 = Counter(mod2, True)
//  val divCount1 = Counter(div1, True)
//  val divCount2 = Counter(div2, True)
//
//
//  val countEscape = Counter(escape, True)
//
//  when(count.willOverflow)(baseReg := U(0))
//    .elsewhen(divCount2.willOverflow && divCount1.willOverflow)(baseReg := baseReg + U(2))
//    .elsewhen(divCount2.willOverflow)(baseReg := baseReg + U(1))
//    .elsewhen(divCount1.willOverflow)(baseReg := baseReg + U(1))
//
//  when(count.willOverflow) {
//    modCount1.clear()
//    modCount2.clear()
//    divCount1.clear()
//    divCount2.clear()
//  }
//
//  val outputReg = Reg(UInt(7 bits)) init (1)
//
//  when(countEscape === U(0))(outputReg := U(0))
//    .otherwise(outputReg := baseReg + modCount1 + modCount2 + 1)
//
//  io.output := outputReg
//}
//
//object testArithmeticSeqGen {
//
//  val period = 2
//
//  def main(args: Array[String]): Unit = {
//    SimConfig.withWave.compile(new ArithmeticSeqGen(5, 9, 3, 7,  13, 100)).
//      doSimUntilVoid { dut =>
//        val clockThread = fork {
//          dut.clockDomain.forkStimulus(period = period)
//        }
//        val mainThread = fork {
//          // test vectors
//          sleep(1000 * period)
//          simSuccess()
//        }
//      }
//  }
//}
