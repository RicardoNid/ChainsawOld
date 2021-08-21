import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

package object FTN {

  // precision of fixed-point number
  val resolution = -6
  val peak = 1
  val wordWidth = 1 + peak - resolution

  def toSFix(value: Double) = SF(value, peak exp, resolution exp)

  val fixedType = HardType(SFix(peak exp, resolution exp))
  val complexType = HardType(ComplexNumber(peak, resolution))

}
