package xilinx

import spinal.core._
import XilinxDeviceFamily._

import scala.collection.mutable.ArrayBuffer

case class VivadoConfig(
                         vivadoPath: String = "/tools/Xilinx/Vivado/2021.1/bin",
                         processorCount: Int = 10
                       )

case class XilinxDevice(
                         family: XilinxDeviceFamily,
                         part: String,
                         fmax: HertzNumber,
                         constraint: String = null
                       )

case class VivadoConstraint(commands: ArrayBuffer[String] = ArrayBuffer[String]()) {

  def setLoc(loc: String, port: String) = {
    commands :+ s"set_property LOC $loc [get_cells $port]"
    this
  }

  def setIOStandard(port: String) = {
    //    commands :+ s"set_property LOC $loc [get_cells $port]"
    this
  }
}