package projects.Hwj

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import sysu.xilinx._
import sysu.util._
import sysu.CNN._

// (a * b) matrix * (b * c) matrix
class AccFIFO(dataWidth: Int, latency: Int) extends Component {

  val io = new Bundle {
    val dataIn = in UInt (dataWidth bits)
    val dataOut = out UInt (dataWidth bits)
  }

  val counterInner = Counter(latency, True)
  val counterOuter = Counter(latency, counterInner.willOverflow)


  val input = UInt(dataWidth bits)
  val delay = Delay(input, latency)
  when(counterOuter.value === 0)(input := io.dataIn)
    .otherwise(input := io.dataIn + delay)

  io.dataOut := delay
  println("latency = ", LatencyAnalysis(io.dataIn, io.dataOut))
}

object AccFIFO {
  def main(args: Array[String]): Unit = {
    if (args(0) == "synth") {
      val report = VivadoFlow(
        design = new AccFIFO(200, 128),
        vivadoConfig = recommended.vivadoConfig,
        vivadoTask = VivadoTask(topModuleName = "AccFIFO", workspacePath = "output/AccFIFO", frequencyTarget = 600 MHz, taskType = SYNTH),
        force = true).doit()
      report.printArea
      report.printFMax
    }
    else if (args(0) == "sim") {
      val period = 2
      SimConfig.withWave.compile(new AccFIFO(200, 128))
        .doSimUntilVoid { dut =>
          fork {
            dut.clockDomain.forkStimulus(period = period)
          }
          sleep(period * 17)
          for (i <- 0 until 12800) {
            dut.io.dataIn #= i
            sleep(period)
          }
          simSuccess()
        }
    }
  }
}




