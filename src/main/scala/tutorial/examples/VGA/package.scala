package tutorial.examples

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

package object VGA {

  case class RgbConfig(rWidth: Int = 8, // RGB位宽配置
                       gWidth: Int = 8,
                       bWidth: Int = 8
                      ) {
    def getWidth = rWidth + gWidth + bWidth
  }

  case class Rgb(c: RgbConfig) extends Bundle {
    val r = UInt(c.rWidth bit)
    val g = UInt(c.gWidth bit)
    val b = UInt(c.bWidth bit)
  }

  case class Vga(rgbConfig: RgbConfig) extends Bundle with IMasterSlave {
    val vSync = Bool
    val hSync = Bool

    val colorEn = Bool
    val color = Rgb(rgbConfig)

    override def asMaster(): Unit = this.asOutput()
  }

  // redundant for vertical and horizontal
  //  case class VgaTimings(timingsWidth: Int) extends Bundle {
  //    val hSyncStart = UInt(timingsWidth bits)
  //    val hSyncEnd = UInt(timingsWidth bits)
  //    val hColorStart = UInt(timingsWidth bits)
  //    val hColorEnd = UInt(timingsWidth bits)
  //    val vSyncStart = UInt(timingsWidth bits)
  //    val vSyncEnd = UInt(timingsWidth bits)
  //    val vColorStart = UInt(timingsWidth bits)
  //    val vColorEnd = UInt(timingsWidth bits)
  //  }

  case class VgaTimingsHV(timingWidth: Int) extends Bundle {
    val colorStart = UInt(timingWidth bit)
    val colorEnd = UInt(timingWidth bit)
    val syncStart = UInt(timingWidth bit)
    val syncEnd = UInt(timingWidth bit)
  }

  case class VgaTimings(timingWidth: Int) extends Bundle {
    val h = VgaTimingsHV(timingWidth)
    val v = VgaTimingsHV(timingWidth)

    def setAs_h640_v480_r60: Unit = {
      h.syncStart := 96 - 1
      h.syncEnd := 800 - 1
      h.colorStart := 96 + 16 - 1
      h.colorEnd := 800 - 48 - 1
      v.syncStart := 2 - 1
      v.syncEnd := 525 - 1
      v.colorStart := 2 + 10 - 1
      v.colorEnd := 525 - 33 - 1
    }

    def setAs_h64_v64_r60: Unit = {
      h.syncStart := 96 - 1
      h.syncEnd := 800 - 1
      h.colorStart := 96 + 16 - 1 + 288
      h.colorEnd := 800 - 48 - 1 - 288
      v.syncStart := 2 - 1
      v.syncEnd := 525 - 1
      v.colorStart := 2 + 10 - 1 + 208
      v.colorEnd := 525 - 33 - 1 - 208
    }

    //    def setAs_h1600_v900_r60 = {
    //
    //    }
  }

}
