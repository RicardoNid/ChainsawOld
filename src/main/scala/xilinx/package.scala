import spinal.core._
import org.apache.commons.io.FileUtils

import java.io.File

package object xilinx {

  // vivado path of your environment
  // TODO: write a test for this
  var vivadoPath = "/tools/Xilinx/Vivado/2021.1/bin"

  val XilinxClockConfig = ClockDomainConfig(resetKind = BOOT)

  sealed trait VivadoTaskType

  object ELABO extends VivadoTaskType

  object SYNTH extends VivadoTaskType

  object IMPL extends VivadoTaskType

  object XilinxDeviceFamily extends Enumeration {
    type XilinxDeviceFamily = Value
    val UltraScale, Series7 = Value
  }

  object XilinxClockDomainConfig

  import XilinxDeviceFamily._

  def xilinxCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = HIGH,
    softResetActiveLevel = HIGH,
    clockEnableActiveLevel = HIGH)

  val vu9p = XilinxDevice(UltraScale, "xcvu9p-flga2104-2-i", 288 MHz)
  val zybo = XilinxDevice(Series7, "xc7z010", 125 MHz, constraint = FileUtils.readFileToString(new File("./src/main/resources/zybo.xdc")))
  //  val zcu104 = XilinxDevice(Series7, "xczu7ev-ffvc1156-2-e", 200MHz)
}
