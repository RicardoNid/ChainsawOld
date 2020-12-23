package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import sysu.xilinx._

package object pipelined {

  val vivadoConfig = VivadoConfig(
    vivadoPath = "C:/Xilinx/Vivado/2020.1/bin",
    deviceFamily = UltraScale,
    devicePart = "xczu7ev-ffvc1156-2-e",
    processortCount = 10
  )

}
