package Chainsaw.FloPoCo

import spinal.core._

abstract class FloPoCoBlackBox[inputType <: Data, outputType <: Data] extends BlackBox {
  val input: inputType
  val output: outputType

  val operatorName: String
  val blackBoxName :String

  def rtlPath = defaultOutputDir + s"/$blackBoxName.vhdl"
}
