package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

case class BitGenFTN() extends Component {
  val pF = pFNonIter
  val ROMValues = DSPRand.nextBinaryString(pF * 1)
}
