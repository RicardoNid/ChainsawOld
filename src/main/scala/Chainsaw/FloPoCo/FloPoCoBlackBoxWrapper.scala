package Chainsaw.FloPoCo

import spinal.core._

abstract class FloPoCoBlackBoxWrapper[inputType <: Data, outputType <: Data] extends Component {
  val input: inputType
  val output: outputType
  val blackBox: BlackBox with FloPoCoBlackBox[inputType, outputType]

  def connect(): Unit = {
    blackBox.input <> input
    blackBox.output <> output
  }
}
