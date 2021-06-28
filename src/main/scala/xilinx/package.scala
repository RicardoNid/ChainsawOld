import spinal.core._

package object xilinx {

  // vivado path of your environment
  // TODO: write a test for this
  var vivadoPath = "/tools/Xilinx/Vivado/2019.2/bin"

  sealed trait TaskType

  object ELABO extends TaskType

  object SYNTH extends TaskType

  object IMPL extends TaskType

  object XilinxDeviceFamily extends Enumeration {
    type XilinxDeviceFamily = Value
    val UltraScale, Series7 = Value
  }

  import XilinxDeviceFamily._

  def xilinxCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = HIGH,
    softResetActiveLevel = HIGH,
    clockEnableActiveLevel = HIGH)

  val defaultVivadoConfig = VivadoConfig( // default vivadoConfig for linux
    xilinxDeviceFamily = UltraScale,
    //    xilinxDeviceFamily = Series7,
    devicePart = "xczu7ev-ffvc1156-2-e", // ZCU104
    //    devicePart = "xc7vx690tffg1761-2", // VC709
    processortCount = 10
  )
}
