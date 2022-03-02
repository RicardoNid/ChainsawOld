package Chainsaw.comm.qam

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class AdaptiveQamdemod(bitCandidates: Seq[Int], hardType: HardType[SFix]) extends Component {

  val bitWidth = bitCandidates.max

  val orderIn = in UInt (log2Up(bitCandidates.max) bits) // modulation order
  val symbolIn = in(ComplexNumber(hardType))
  val bitsOut = out Bits (bitWidth bits)

  val realType = symbolIn.realType

  // number of stages needed to judge
  val stageReal = (bitWidth + 1) / 2
  val stageImag = bitWidth / 2

  // building rom of thresholds
  //  def getThresholds(alloc:Int) =  (0 until (alloc + 1) / 2 - 1).map()


  // generate determinant bool values according to number of stages
  def genDeterminants(value: SFix, stage: Int) = {

  }

}
