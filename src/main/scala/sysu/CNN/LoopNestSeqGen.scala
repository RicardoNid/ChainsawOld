//package sysu.CNN
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.core.sim._
//import scala.util.Random
//
//import sysu.xilinx._
//import sysu.util._
//
//class LoopNestSeqGen(loopNestCNN: LoopNestCNN) extends Component {
//
//  import loopNestCNN._
//
//  val io = new Bundle {
//    val output = out UInt (12 bits)
//  }
//
//  val CountKx = Counter(Tkx, True)
//  val CountKy = Counter(Tky, CountKx.willOverflow)
//  val CountKx = Counter(Tkx, True)
//  val CountKx = Counter(Tkx, True)
//  val CountKx = Counter(Tkx, True)
//  val CountKx = Counter(Tkx, True)
//}
//
//object LoopNestSeqGen {
//  def main(args: Array[String]): Unit = {
//    val moduleName = "LoopNestSeqGen"
//    val report = VivadoFlow(
//      design = new LoopNestSeqGen(),
//      vivadoConfig = recommended.vivadoConfig,
//      vivadoTask = VivadoTask(
//        topModuleName = moduleName,
//        workspacePath = s"output/LoopNestSeqGen/LoopNestSeqGen")).doit()
//    report.printArea
//    report.printFMax
//  }
//}
//
//object testLoopNestSeqGen {
//
//  val period = 2
//
//  def main(args: Array[String]): Unit = {
//    SimConfig.withWave.compile(new LoopNestSeqGen).
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
