//package projects.Hwj
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.lib.bus.bram._
//import spinal.core.sim._
//
//import sysu.xilinx._
//import sysu.util._
//import sysu.CNN._
//
//// (a * b) matrix * (b * c) matrix
//class DataBuf(dataWidth: Int, conv: ToeplitzConv, next: ToeplitzConv) extends Component {
//
//  import conv._
//
//  val io = new Bundle {
//    val dataBufIn = slave(Stream(in(BundleCHW(next))))
//    val dataBufOut = master(Stream(in(BundleCHW(next))))
//  }
//
//  io.dataBufIn.ready := io.dataBufOut.ready
//  io.dataBufOut.valid := io.dataBufIn.valid
//}
//
//object DataBuf {
//  def main(args: Array[String]): Unit = {
//    if (args(0) == "synth") {
//      val report = VivadoFlow(
//        design = new DataBuf(dataWidth, res2_1_a, res2_1_b),
//        vivadoConfig = recommended.vivadoConfig,
//        vivadoTask = VivadoTask(topModuleName = "DataBuf", workspacePath = "output/DataBuf", frequencyTarget = 600 MHz, taskType = SYNTH),
//        force = true).doit()
//      report.printArea
//      report.printFMax
//    }
//    else if (args(0) == "sim") {
//      val period = 2
//      SimConfig.withWave.compile(new DataBuf(dataWidth, res2_1_a, res2_1_b))
//        .doSimUntilVoid { dut =>
//          fork {
//            dut.clockDomain.forkStimulus(period = period)
//          }
//          sleep(period * 17)
//          sleep(period / 2)
//
//          sleep(period * 50)
//          simSuccess()
//        }
//    }
//  }
//}
//
//
//
//
