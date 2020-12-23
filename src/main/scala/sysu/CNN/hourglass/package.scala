package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import sysu.xilinx._

package object hourglass {
  val vivadoConfig = VivadoConfig(
    vivadoPath = "",
    deviceFamily = UltraScale,
    devicePart = "",
    processortCount = 0
  )
}
