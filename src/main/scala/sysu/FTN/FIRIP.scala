package sysu.FTN

import FIRIP.config
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi.Axi4

import scala.util.Random

case class FIRIPConfig(
                        dataWidth: Int = 8,
                        userWidth: Int = 8,
                        hasClken: Boolean = false,
                        hasResetn: Boolean = false,
                        hasLast: Boolean = false,
                        hasUser: Boolean = false,
                        hasConfig: Boolean = false,
                        hasReload: Boolean = false,
                        coeffs: Array[Int] = default_vector
                      )

class FIRIP(FIRConfig: FIRIPConfig) extends Component {

  val bitGrowth = log2Up(config.coeffs.reduce(_ + _))

  val io = new Bundle {
    val aclk = in Bool
    val aclken = if (FIRConfig.hasClken) in Bool else null
    val aresetn = if (FIRConfig.hasResetn) in Bool else null
    val config = if (FIRConfig.hasConfig) slave(AXIS(1, FIRConfig.hasLast, 0)) else null
    val reload = if (FIRConfig.hasReload) slave(AXIS(8, FIRConfig.hasLast, 0)) else null
    val datain = slave(AXIS(FIRConfig.dataWidth, FIRConfig.hasLast, FIRConfig.userWidth))
    val dataout = master(AXIS(FIRConfig.dataWidth + bitGrowth, FIRConfig.hasLast, FIRConfig.userWidth))
  }

  // 设定时钟域
  val CD = ClockDomain(
    clock = io.aclk,
    reset = io.aresetn,
    clockEnable = io.aclken,
    config = CDConfig
  )

  val mainArea = new ClockingArea(CD) {
    io.datain.stream.ready := True

    //    io.dataout.stream.payload := io.datain.stream.payload.resized

    // 暂时不实现config和reload,填充config和reload通道的输出
    if (FIRConfig.hasConfig) io.config.stream.ready := True
    if (FIRConfig.hasReload) io.reload.stream.ready := True

    // 实现运算功能
    val xReg = RegNext(io.datain.stream.payload)

    // FSM
    val calculationFSM = new StateMachine {

      val numWorkingCycle = roundUp(FIRConfig.coeffs.length, 2) / 2
      println("working cycle = " + numWorkingCycle)

      val IDLE = new State with EntryPoint
      val BUSY1 = new StateDelay(cyclesCount = numWorkingCycle)
      val BUSY2 = new StateDelay(cyclesCount = numWorkingCycle)


      io.dataout.stream.valid := False
      io.dataout.stream.payload := U(10).asBits.resized // 只是占位

      stateBoot
        .whenIsActive {
          goto(IDLE)
        }

      IDLE
        .whenIsActive(when(io.datain.stream.fire)(goto(BUSY1)))

      BUSY1
        .whenCompleted {
          io.dataout.stream.valid := True
          when(io.datain.stream.fire)(goto(BUSY2)).otherwise(goto(IDLE))
        }

      BUSY2
        .whenCompleted {
          io.dataout.stream.valid := True
          when(io.datain.stream.fire)(goto(BUSY1)).otherwise(goto(IDLE))
        }
    }

    calculationFSM.setPartialName("FSM")
  }


  noIoPrefix()
}

object FIRIP {

  val outputDir = "C:/Users/lsfan/FIR_IP/FIR_IP.srcs"

  val config = FIRIPConfig(
    dataWidth = 16,
    userWidth = 0)

  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = outputDir)
      .generate(new FIRIP(
        FIRIPConfig(
        dataWidth = 16,
        userWidth = 0))).printPruned().printPrunedIo()
  }
}

object testFIRIP {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new FIRIP(FIRIPConfig())).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          // test vectors
          simSuccess()
        }
      }
  }
}
