package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import matlabIO._

class Tx(parallelFactor: Int) extends Component {
  val dataIn = Bits(parallelFactor bits)

  val convencConfig = ConvencConfig(7, Array(171, 133))
  val convencFTN = ConvencFTN(convencConfig, parallelFactor)

  convencFTN.dataIn := dataIn
  val coded = convencFTN.dataOut

  val interleaverFTN = InterleaverFTN(32, 128, parallelFactor * 2)
  interleaverFTN.dataIn := coded
  val interleaved = interleaverFTN.dataOut

  val bitAlloc = Array.fill(parallelFactor)(4)
  val qammodFTN = QammodFTN(bitAlloc, parallelFactor)
  qammodFTN.dataIn := interleaved
  val dataOut = qammodFTN.dataOut
}
