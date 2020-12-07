//package examples.UART
//
//import spinal.core._
//import spinal.lib._
//import spinal.core.sim._
//import scala.util.Random
//
//class UartCtrlTx(g: UartCtrlGenerics) extends Component {
//
//  import g._
//
//  val io = new Bundle {
//    val configFrame = in(UartCtrlFrameConfig(g))
//    val samplingTick = in Bool
//    val write = slave Stream (Bits(dataWidthMax bit))
//    val txd = out Bool
//  }
//
//  // Provide one clockDivider.tick each rxSamplePerBit pulse of io.samplingTick
//  // Used by the stateMachine as a baud rate time reference
//  val clockDivider = new Area {
//    val counter = Reg(UInt(log2Up(rxSamplePerBit) bits)) init (0)
//    val tick = False
//    when(io.samplingTick) {
//      counter := counter - 1
//      tick := counter === 0
//    }
//  }
//
//  // Count up each clockDivider.tick, used by the state machine to count up data bits and stop bits
//  val tickCounter = new Area {
//    val value = Reg(UInt(Math.max(dataWidthMax, 2) bit))
//
//    def reset() = value := 0
//
//    when(clockDivider.tick) {
//      value := value + 1
//    }
//  }
//
//  val stateMachine = new Area {
//
//    import UartCtrlTxState._
//
//    val state = RegInit(IDLE)
//    val parity = Reg(Bool)
//    val txd = True
//
//    when(clockDivider.tick) {
//      parity := parity ^ txd
//    }
//
//    io.write.ready := False
//    switch(state) {
//      is(IDLE) {
//        when(io.write.valid && clockDivider.tick) {
//          state := START
//        }
//      }
//      is(START) {
//        txd := False
//        when(clockDivider.tick) {
//          state := DATA
//          parity := io.configFrame.parity === UartParityType.ODD
//          tickCounter.reset()
//        }
//      }
//      is(DATA) {
//        txd := io.write.payload(tickCounter.value)
//        when(clockDivider.tick) {
//          when(tickCounter.value === io.configFrame.dataLength) {
//            io.write.ready := True
//            tickCounter.reset()
//            when(io.configFrame.parity === UartParityType.NONE) {
//              state := STOP
//            } otherwise {
//              state := PARITY
//            }
//          }
//        }
//      }
//      is(PARITY) {
//        txd := parity
//        when(clockDivider.tick) {
//          state := STOP
//          tickCounter.reset()
//        }
//      }
//      is(STOP) {
//        when(clockDivider.tick) {
//          when(tickCounter.value === toBitCount(io.configFrame.stop)) {
//            state := io.write.valid ? START | IDLE
//          }
//        }
//      }
//    }
//  }
//
//  io.txd := RegNext(stateMachine.txd, True)
//}
//
//object UartCtrlTx {
//  def main(args: Array[String]): Unit = {
//    SpinalConfig(mode = SystemVerilog, targetDirectory = projectSrcs).generate(new UartCtrlTx)
//  }
//}
//
//object testUartCtrlTx {
//
//  val period = 2
//
//  def main(args: Array[String]): Unit = {
//    SimConfig.withWave.compile(new UartCtrlTx).
//      doSimUntilVoid { dut =>
//        val clockThread = fork {
//          dut.clockDomain.forkStimulus(period = period)
//        }
//        val mainThread = fork {
//          // test vectors
//          simSuccess()
//        }
//      }
//  }
//}
