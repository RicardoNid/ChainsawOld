package Chainsaw.DSP

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class ComplexMult()
  extends Component with DSPTestable[Vec[ComplexNumber], ComplexNumber] {

  override val dataIn = slave Stream Vec(ComplexNumber(7, -8), 2)
  override val dataOut = master Stream ComplexNumber(7, -8)
  override val latency = cmultConfig.pipeline
  dataOut.payload := (dataIn.payload(0) * dataIn.payload(1)).truncated(dataOut.payload.realType)
  dataOut.valid := True
  dataIn.ready := True
}

object ComplexMult extends App {
  VivadoSynthForTiming(ComplexMult(), "complexMultNotTruncated") // 4 dsp
  cmultConfig = ComplexMultConfig(true, 3, ComplexNumber(7, -8).realType)
  VivadoSynthForTiming(ComplexMult(), "complexMultTruncated") // 3 dsp
}
