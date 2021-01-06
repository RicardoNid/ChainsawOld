//package sysu.util
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
//import sysu.Opt._
//
//import sysu.CNN.ToeplitzConv._
//
//// 这个模块的实现,很多前提条件隐藏在端口划分算法中,硬件和算法的耦合度可能太高了
//class MemAccess(dataWidth: Int,
//                memAccessMapIn: Array[Array[Tuple2[Int,Int]]],
//                memAccessMapOut: Array[Array[Tuple2[Int,Int]]]) extends Component {
//
//  val inputWidth = memAccessMapIn(0).size
//  val inputPeriod = memAccessMapIn.size
//  val outputWidth = memAccessMapOut(0).size
//  val outputPeriod = memAccessMapOut.size
//  val ports = memAccessMapIn.flatten.map(_._1).distinct.max
//  val depth = memAccessMapIn.flatten.map(_._2).distinct.max
//  val inputPortSeqs = transpose2D(memAccessMapIn.map(_.map(_._1)))
//  val inputAddrSeqs = transpose2D(memAccessMapIn.map(_.map(_._2)))
//  val outputPortSeqs = transpose2D(memAccessMapOut.map(_.map(_._1)))
//  val outputAddrSeqs = transpose2D(memAccessMapOut.map(_.map(_._2)))
//
//  val io = new Bundle {
//    val input = slave(Stream(Fragment(Vec(Bits(dataWidth bits), inputWidth))))
//    val output = master(Stream(Fragment(Vec(Bits(dataWidth bits), outputWidth))))
//  }
//
//  val inputCrossGens = inputPortSeqs.map(seq => SeqGen(seq, LUT))
//  val inputAddrGens = inputAddrSeqs.map(seq => SeqGen(seq, LUT))
//  val outputPortGens = outputPortSeqs.map(seq => SeqGen(seq, LUT))
//  val outputAddrGens = outputAddrSeqs.map(seq => SeqGen(seq, LUT))
//  val mems = (0 until ports).map(_ => Mem(Bits(dataWidth bits), depth))
//
//  // 访存控制
//  val memsInput = Vec(Bits(dataWidth bits), ports)
//  val memsOutput = Vec(Bits(dataWidth bits), ports)
//  memsOutput.foreach(_ := mems)
//
//  // crossbar连接
//  val crossedOutput = CrossBar(memsOutput, outputPortGens.map(_.implicitValue.asBits))
//  io.output.payload.fragment := crossedOutput
//
//  // crossbar数据生成
//  // 地址数据生成
//  // Mem
//  // 传输会话控制
//
//
//
//}
//
////object MemAccess {
////  def main(args: Array[String]): Unit = {
////    if (args(0) == "synth") {
////      val report = VivadoFlow(
////        design = new MemAccess(),
////        vivadoConfig = recommended.vivadoConfig,
////        vivadoTask = VivadoTask(topModuleName = "MemAccess", workspacePath = "output/MemAccess", frequencyTarget = 600 MHz, taskType = SYNTH),
////        force = true).doit()
////      report.printArea
////      report.printFMax
////    }
////    else if (args(0) == "sim") {
////      val period = 2
////      SimConfig.withWave.compile(new MemAccess())
////        .doSimUntilVoid { dut =>
////          fork {
////            dut.clockDomain.forkStimulus(period = period)
////          }
////          sleep(period * 17)
////          sleep(period / 2)
////
////          sleep(period * 50)
////          simSuccess()
////        }
////    }
////  }
////}
//
//
//
//
