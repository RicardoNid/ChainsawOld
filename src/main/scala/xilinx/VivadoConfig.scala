package xilinx

import XilinxDeviceFamily._
case class VivadoConfig(
    xilinxDeviceFamily: XilinxDeviceFamily = UltraScale,
    devicePart: String,
    processortCount: Int
)
