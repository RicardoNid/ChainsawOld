package tutorial.examples.VGA

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class VgaCtrl(rgbConfig: RgbConfig = new RgbConfig(8, 8, 8), timingsWidth: Int = 12) extends Component {
  val io = new Bundle {
    val softReset = in Bool
    val timings = in(VgaTimings(timingsWidth))
    val pixels = slave Stream (Rgb(rgbConfig))

    val error = out Bool
    val frameStart = out Bool
    val vga = master(Vga(rgbConfig))
  }

  case class HVArea(timingsHV: VgaTimingsHV, enable: Bool) extends Area {
    val counter = Reg(UInt(timingsWidth bit)) init (0)

    val syncStart = counter === timingsHV.syncStart
    val syncEnd = counter === timingsHV.syncEnd
    val colorStart = counter === timingsHV.colorStart
    val colorEnd = counter === timingsHV.colorEnd

    when(enable) {
      counter := counter + 1
      when(syncEnd) {
        counter := 0
      }
    }

    val sync = RegInit(False) setWhen (syncStart) clearWhen (syncEnd)
    val colorEn = RegInit(False) setWhen (colorStart) clearWhen (colorEnd)

    when(io.softReset) {
      counter := 0
      sync := False
      colorEn := False
    }
  }

  val h = HVArea(io.timings.h, True)
  val v = HVArea(io.timings.v, h.syncEnd)

  val colorEn = h.colorEn && v.colorEn
  io.pixels.ready := colorEn
  io.error := colorEn && io.pixels.valid
  io.frameStart := v.syncEnd
  io.vga.hSync := h.sync
  io.vga.vSync := v.sync
  io.vga.colorEn := colorEn
  io.vga.color := io.pixels.payload
}

object VgaCtrl {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog).generate(new VgaCtrl)
  }
}

object testVgaCtrl {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new VgaCtrl).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          sleep(period / 2)
          dut.io.softReset #= true
          sleep(period)
          dut.io.softReset #= false
          dut.io.pixels.valid #= true
          dut.io.timings.h.syncStart #= 96 - 1
          dut.io.timings.h.syncEnd #= 800 - 1
          dut.io.timings.h.colorStart #= 96 + 16 - 1
          dut.io.timings.h.colorEnd #= 800 - 48 - 1
          dut.io.timings.v.syncStart #= 2 - 1
          dut.io.timings.v.syncEnd #= 525 - 1
          dut.io.timings.v.colorStart #= 2 + 10 - 1
          dut.io.timings.v.colorEnd #= 525 - 33 - 1
          dut.io.pixels.payload.r #= 127
          dut.io.pixels.payload.g #= 127
          dut.io.pixels.payload.b #= 127
          for(i <- 0 until 5000){
            sleep(period)
          }

          simSuccess()
        }
      }
  }
}
