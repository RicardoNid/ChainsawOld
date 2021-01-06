//package sysu.CNN
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.core.sim._
//import sysu.util._
//import sysu.xilinx._
//import sysu.CNN._
//import projects.FTN.Interleave
//
//class ConvSeqGen(loopNestConv: LoopNestConv,
//                 definition: Map[LoopVar, Array[Int]] = Map(
//                   OF -> Array(0, 0),
//                   IF -> Array(0, 0),
//                   OY -> Array(0, 0),
//                   OX -> Array(0, 0),
//                   KY -> Array(0, 0),
//                   KX -> Array(0, 3)
//                 )) extends Component {
//
//  import loopNestConv._
//
//  val io = new Bundle {
//    val output = out UInt (16 bits)
//  }
//
//  U"0"
//
//  val tilePre = Array(EMPTY) ++ tileOrder.dropRight(1)
//  val tileNext2Pre = tileOrder.zip(tilePre).map {
//    case (pre, next) => pre -> next
//  }.toMap
//
//  val counters = Array(Tkx, Tky, Tox, Toy, Tif, Tof).map(Counter(_))
//  Array(KX, KY, OX, OY, IF, OF).foreach { ele =>
//    val pre = tileNext2Pre(ele)
//    val preIndex = tileOrder.indexOf(pre)
//    val eleIndex = tileOrder.indexOf(ele)
//    if (pre != EMPTY) when(counters(preIndex).willOverflow)(counters(eleIndex).increment())
//    else counters(eleIndex).increment()
//  }
//
//  io.output := counters.map(_.value).reduce(_.resize(16) + _.resize(16))
//}
//
//object ConvSeqGen {
//  def main(args: Array[String]): Unit = {
//    val report = VivadoFlow(design = new ConvSeqGen(
//      LoopNestConv()),
//      vivadoConfig = recommended.vivadoConfig,
//      vivadoTask = VivadoTask(topModuleName = "ConvSeqGen", workspacePath = "output/ConvSeqGen", taskType = SYNTH),
//      force = true).doit()
//
//    report.printFMax
//    report.printArea
//  }
//}
//
//object testConvSeqGen {
//
//  val period = 2
//
//  def main(args: Array[String]): Unit = {
//    SimConfig.withWave.compile(new ConvSeqGen(LoopNestConv())).
//      doSimUntilVoid { dut =>
//        val clockThread = fork {
//          dut.clockDomain.forkStimulus(period = period)
//        }
//
//        sleep(16 * period)
//        sleep(period / 2)
//        sleep(100000 * period)
//        simSuccess()
//      }
//  }
//}
//
