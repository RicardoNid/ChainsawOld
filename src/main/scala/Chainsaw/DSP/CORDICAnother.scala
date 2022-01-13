package Chainsaw.DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

//case class CORDICAnother(cordicConfig: CordicConfig)
//  extends Component with DSPTestable[Vec[SFix], Vec[SFix]] {
//
//  val magnitudeType = HardType(SFix(1 exp, -cordicConfig.precision exp))
//  val phaseType = HardType(SFix(2 exp, -cordicConfig.precision exp))
//
////  override val dataIn = Vec() // x, y, z
////  override val dataOut = _ // x, y, z
////  override val latency = _
//  override type RefOwnerType = this.type
//}
