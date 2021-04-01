package xilinx

import spinal.core._

object recommended {
  def XilinxCDConfig = ClockDomainConfig( // recommended by Xilinx UG901
    clockEdge = RISING,
    resetKind = ASYNC,
    resetActiveLevel = HIGH,
    softResetActiveLevel = HIGH,
    clockEnableActiveLevel = HIGH)

  val vivadoConfig = VivadoConfig( // default vivadoConfig for linux
    vivadoPath = "/tools/Xilinx/Vivado/2019.2/bin",
    deviceFamily = UltraScale,
    devicePart = "xczu7ev-ffvc1156-2-e", // ZCU104
    processortCount = 10
  )

  val synthStrategy = SynthStrategy(
    flatten_hierarchy = FlattenHierarchy.none
  )
}
