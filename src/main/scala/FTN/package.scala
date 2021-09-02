import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._

package object FTN {


  def params: MStruct = {
    eng.eval("cd /home/ltr/IdeaProjects/Chainsaw/matlabWorkspace/FTN326; \n" +
      "channel = 3:226; \n" +
      "InitPARAMS(2, channel); \n" +
      "load PARAMS PARAMS")
    eng.getVariable[MStruct]("PARAMS")
  }

  val convencConfig = ConvencConfig(7, Array(171, 133))

  val channelCount = 512
  val runningChannelCount = 448

  val OFDMSymbolPerFrame = 8
  val bitsPerSymbol = 4

  //  val bitsPerFrame = OFDMSymbolPerFrame * 2 * channelCount

  val interleaveDepth = channelCount * 8
  val interleaveCol = 128
  val interleaveRow = interleaveDepth / interleaveCol

  // precision of fixed-point number
  val resolution = -7
  val peak = 4
  val wordWidth = 1 + peak - resolution

  def toSFix(value: Double) = SF(value, peak exp, resolution exp)

  val fixedType = HardType(SFix(peak exp, resolution exp))
  val coeffType = HardType(SFix(1 exp, -10 exp))
  val complexType = HardType(ComplexNumber(peak, resolution))

  val frameBitsCount = 16 * runningChannelCount
  val frameBitsCountPadded = 16 * runningChannelCount

  val pFNonIter = 64 // pF for non-iterative part, denoted as the pF of initially issued bits(from BitGen)
  val pFIter = 512
}
