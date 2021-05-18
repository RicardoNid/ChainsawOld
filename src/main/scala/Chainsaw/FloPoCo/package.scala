package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

package object FloPoCo {
  val flopocoPath = "/home/ltr/FloPoCo/flopoco/build"
  val defaultOutputPath = flopocoPath + "/flopoco.vhdl"
  val defaultOutputDir = "/home/ltr/IdeaProjects/Chainsaw/tempRTL"

  case class FloPoCoConfig(entityName: String, outputFile: String)
}
