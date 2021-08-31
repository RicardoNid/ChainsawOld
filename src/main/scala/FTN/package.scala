import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

package object FTN {

  val convencConfig = ConvencConfig(7, Array(171, 133))

  val channelCount = 512

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

  val frameBitsCount = 16 * channelCount

  val pFNonIter = 64 // pF for non-iterative part, denoted as the pF of initially issued bits(from BitGen)
  val pFIter = 512
}
