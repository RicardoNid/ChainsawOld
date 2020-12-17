//package CNN.VGG16
//
//import CNN.inputMapAddrSeq
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.core.sim._
//
//import scala.util.Random
//import glob._
//import mylib.SeqGen
//
//class VGGInputAccessor() extends Component {
//  val io = new Bundle {
//
//  }
//}
//
//object VGGInputAccessor {
//  def main(args: Array[String]): Unit = {
//    val model = CNN.CNNModel()
//    val addr = inputMapAddrSeq(model.C(11), model.H(11), model.W(11), model.K(11), model.W_w(11), model.I_w(11))
//    val sequence1 = addr.map(_ (0))
//
//    println(addr.map(_ (0)).toString())
////
////    SpinalConfig(mode = SystemVerilog, targetDirectory = "output/VGG16").generate(new SeqGen(sequence1))
////    val report = VivadoFlow(
////      // todo : vivado flow infos
////      workspacePath = "output/VIVADO",
////      toplevelPath = "output/VGG16/SeqGen.sv"
////    )
////    println(report.getArea)
////    println(report.getFMax() / 1E6 + "MHz")
//  }
//}
//
//object testVGGInputAccessor {
//
//  val period = 2
//
//  def main(args: Array[String]): Unit = {
//    SimConfig.withWave.compile(new VGGInputAccessor).
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
