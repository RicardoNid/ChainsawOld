package sysu.xilinx

import spinal.core._

object recommended {
  def XilinxCDConfig = ClockDomainConfig( // Xilinx UG901推荐的设计范式
    clockEdge = RISING,
    resetKind = BOOT, // BOOT类型太重要了
    resetActiveLevel = LOW,
    softResetActiveLevel = LOW,
    clockEnableActiveLevel = HIGH)

  val vivadoConfig = VivadoConfig( // 库默认vivadoConfig
    vivadoPath = "C:/Xilinx/Vivado/2020.1/bin",
    deviceFamily = UltraScale,
    devicePart = "xczu7ev-ffvc1156-2-e", // ZCU104
    processortCount = 10
  )

  val synthStrategy = SynthStrategy(
    flatten_hierarchy = FlattenHierarchy.none
  )

  val vivadoTaskTemp = VivadoTask(
    topModuleName = "temp",
    workspacePath = "output/temp",
    frequencyTarget = 600 MHz,
    taskType = SYNTH
  )
}
