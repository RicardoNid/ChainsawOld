package Chainsaw.comm.qam

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class AdaptiveQamdemod(bitCandidates:Seq[Int], hardType: HardType[SFix]) extends Component {

}
