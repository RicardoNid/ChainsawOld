//package basic
//
//import spinal.core._
//import spinal.lib._
//import spinal.core.sim._
//import scala.util.Random
//
//class Timer extends Component  {
//  val io = new Bundle {
//    val tick = out Bool
//  }
//
//  val timer = Timeout(10 MHz)
//  io.tick := timer
//  when(io.tick) {
//    timer.clear()
//  }
//
//  val latency = LatencyAnalysis(timer, io.tick)
//  println("latency: " + latency)
//}
//
//object Timer {
//  def main(args: Array[String]): Unit = {
//    SpinalConfig(mode = SystemVerilog, defaultClockDomainFrequency = FixedFrequency(100 MHz)).generate(new Timer()
//  }
//}
//
//object testTimer {
//  def main(args: Array[String]): Unit = {
//    val config = SpinalConfig(defaultClockDomainFrequency = FixedFrequency(500 MHz))
//    SimConfig.withWave.withConfig(config).compile(new Timer).
//      doSimUntilVoid { dut =>
//        val clockThread = fork {
//          dut.clockDomain.risingEdge()
//          while (true) {
//            dut.clockDomain.clockToggle()
//            sleep(1)
//          }
//        }
//        val mainThread = fork {
//          for (i <- 0 until 100) sleep(2)
//            simSuccess()
//        }
//      }
//  }
//}
