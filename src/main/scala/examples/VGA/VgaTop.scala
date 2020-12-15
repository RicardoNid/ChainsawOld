package examples.VGA

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.util.Random

class VgaTop extends Component {

  val bitWidth = 6
  val rgbConfig = new RgbConfig(5, 6, 5)

  val io = new Bundle {
    val reset = in Bool

    val error = out Bool
    val frameStart = out Bool
    val vga = master(Vga(rgbConfig))
  }

  val ctrl = new VgaCtrl(rgbConfig = rgbConfig)
  ctrl.io.timings.setAs_h640_v480_r60


  val colCount = Counter(0 until 64, ctrl.io.pixels.fire)
  val colBlockCount = Counter(0 until 10, colCount.willOverflowIfInc)
  val rowCount = Counter(0 until 48, colBlockCount.willOverflowIfInc && colCount.willOverflowIfInc)
  val rowBlockCount = Counter(0 until 10, rowCount.willOverflowIfInc && colBlockCount.willOverflowIfInc && colCount.willOverflowIfInc)
  val samples = 100
  val rom = Mem(UInt(rgbConfig.getWidth bits), Range(0, samples).map(_ => U(Random.nextInt(65536))))

  ctrl.io.softReset := io.reset
  io.error := ctrl.io.error
  io.frameStart := ctrl.io.frameStart
  io.vga <> ctrl.io.vga

  ctrl.io.pixels.valid := True
  val pixel = UInt(rgbConfig.getWidth bits)
  pixel := rom.readSync((rowBlockCount * 10 + colBlockCount).resized) // zybo VGA R:G:B = 5:6:5

  ctrl.io.pixels.payload.r := pixel(15 downto 11)
  ctrl.io.pixels.payload.g := pixel(10 downto 5)
  ctrl.io.pixels.payload.b := pixel(4 downto 0)
}

object VgaTop {
  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog, targetDirectory = "C:/Users/lsfan/Documents/GitHub/zybo_vga/zybo_vga.srcs").generate(new VgaTop)
  }
}

object testVgaTop {

  val period = 2

  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new VgaTop).
      doSimUntilVoid { dut =>
        val clockThread = fork {
          dut.clockDomain.forkStimulus(period = period)
        }
        val mainThread = fork {
          sleep(period / 2)
          dut.io.reset #= true
          sleep(period)
          dut.io.reset #= false
          sleep(640 * 480 * 10 * period)
          simSuccess()
        }
      }
  }
}
