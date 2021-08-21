package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

// currently, QAM16
case class QamdemodFTN(pF:Int) extends Component {

  val symbolSize = 4
  val dataIn = slave Stream

}
