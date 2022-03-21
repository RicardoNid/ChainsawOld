package Chainsaw

package object tobeTransplanted {
  val flopocoPath       = "/home/ltr/FloPoCo/flopoco/build"
  val defaultOutputPath = flopocoPath + "/flopoco.vhdl"
  val defaultOutputDir  = "/home/ltr/IdeaProjects/Chainsaw/tempRTL"

  case class FloPoCoConfig(entityName: String, outputFile: String)
}
