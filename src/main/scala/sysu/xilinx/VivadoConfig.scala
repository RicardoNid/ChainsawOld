package sysu.xilinx

trait DeviceFamily
case object UltraScale extends DeviceFamily
case object Series7 extends DeviceFamily

case class VivadoConfig(
                         vivadoPath: String = "C:/Xilinx/Vivado/2020.1/bin",
                         deviceFamily: DeviceFamily = UltraScale,
                         devicePart: String = "xczu7ev-ffvc1156-2-e",
                         processortCount: Int = 10
                       )