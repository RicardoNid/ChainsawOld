package Chainsaw.FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

case class SplitterFTN() extends Component {

  val dataIn = slave Stream smootherComplexVecType
  val dataOut, preambleOut = master Stream smootherComplexVecType

  val counter = Counter(18, inc = dataIn.fire)
  dataIn.ready := (preambleOut.ready && counter < 2) || (dataOut.ready && counter >= 2)

  dataOut.payload := dataIn.payload
  dataOut.valid := (counter >= 2 && dataIn.valid)

  preambleOut.payload := dataIn.payload
  preambleOut.valid := (counter < 2 && dataIn.valid)
}
