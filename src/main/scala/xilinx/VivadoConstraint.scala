package xilinx

import spinal.core._

import scala.collection.mutable.ArrayBuffer

case class VivadoConstraint(commands: Seq[String] = Seq[String]())

case class VivadoConstraintBuilder(commands: Seq[String] = Seq[String]()) {

  def setLoc(loc: String, port: String) = VivadoConstraintBuilder(commands :+ s"set_property LOC $loc [get_cells $port]")
//  def setIOStandard(port:String) = VivadoConstraintBuilder(commands :+ s"set_property LOC $loc [get_cells $port]")
  def build = VivadoConstraint(commands)

}
