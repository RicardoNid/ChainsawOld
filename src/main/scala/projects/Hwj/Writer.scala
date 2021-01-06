//package projects.Hwj
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.lib.bus.bram._
//import spinal.core.sim._
//import sysu.xilinx._
//import sysu.util._
//import sysu.CNN._
//
//// 采用上一层的output参数而非下一层的input参数,因为还需要上一层的Wh,Ww,Iw
//class Writer(dataWidth: Int, conv: ToeplitzConv) extends Component {
//
//  import conv._
//
//  val io = new Bundle {
//    val calcDataOut = slave Stream (Vec(Vec(SInt(dataWidth bits), Iw), Wh))
//    val dataBufDataIn = master(BRAM(BRAMConfig(8, 8)))
//    val dataBufCtrlIn = master Stream (Bits(0 bits))
//    val error = out Bool // for debug
//  }
//
//  val fsm = new StateMachine {
//    val EMPTY = StateEntryPoint()
//    val READ, FULL, WRITE = State()
//
//    EMPTY
//      .whenIsActive(when(io.calcDataOut.valid)(goto(READ)))
//    READ
//      .whenIsActive{
//        when(!io.calcDataOut.valid && io.dataBufCtrlIn.ready)(goto(WRITE))
//          .elsewhen(!io.calcDataOut.valid )(goto(FULL))
//      }
//    FULL
//      .whenIsActive(when(io.dataBufCtrlIn.ready)(goto(WRITE)))
//    WRITE
//      .whenIsActive(when(!))
//
//    io.error := io.calcDataOut.valid && !isActive(EMPTY)
//  }
//
//
//
//}
//
//object Writer {
//  def main(args: Array[String]): Unit = {
//    if (args(0) == "synth") {
//      val report = VivadoFlow(
//        design = new Writer(),
//        vivadoConfig = recommended.vivadoConfig,
//        vivadoTask = VivadoTask(topModuleName = "Writer", workspacePath = "output/Writer", frequencyTarget = 600 MHz, taskType = SYNTH),
//        force = true).doit()
//      report.printArea
//      report.printFMax
//    }
//    else if (args(0) == "sim") {
//      val period = 2
//      SimConfig.withWave.compile(new Writer())
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
