package Chainsaw

import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object DSPInfer extends App {

  case class DSPWithADREG() extends Component with DSPTestable[Vec[SFix], SFix] {
    val hardType = HardType(SFix(7 exp, 16 bits))
    override val dataIn = slave Flow Vec(hardType, 4)
    override val dataOut = master Flow hardType()

    val Seq(a, b, c, d) = dataIn.payload
    val ret = ((a +^ d).d * b.d).d + c.d(2) // using adreg but not preg

    dataOut.payload := ret.truncated
    dataOut.valid := dataIn.valid.d(2)

    override val latency = 2
  }

  case class DSPWithPREG() extends Component with DSPTestable[Vec[SFix], SFix] {
    val hardType = HardType(SFix(7 exp, 16 bits))
    override val dataIn = slave Flow Vec(hardType, 4)
    override val dataOut = master Flow hardType()

    val Seq(a, b, c, d) = dataIn.payload
    val ret = (((a +^ d) * b).d + c.d).d // using preg but not adreg

    dataOut.payload := ret.truncated
    dataOut.valid := dataIn.valid.d(2)

    override val latency = 2
  }

  VivadoSynthForTiming(DSPWithPREG(), "dspWithPReg")
  VivadoSynthForTiming(DSPWithADREG(), "dspWithADReg")

}
