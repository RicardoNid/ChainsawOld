package Chainsaw

import spinal.core._

case class BitAllocator() extends Component {

  val dataIn = in Bits (1024 bits)
  val shiftIn = in UInt (9 bits)
  val dataOut = out Bits (1024 bits)

  dataOut := RegNext(dataIn.rotateLeft(shiftIn))
}

object BitAllocator extends App {
  //    VivadoImpl(BitAllocator())
  val sfix16 = HardType(SFix(7 exp, -8 exp))
  //  VivadoImpl(DSP.FFT.DFT(2, false, sfix16, sfix16))
  VivadoSynthForTiming(DSP.FFT.AdaptiveCooleyTukeyFFT(64, 16, false, sfix16, sfix16, Seq(4, 4, 4)))
  VivadoImpl(DSP.FFT.AdaptiveCooleyTukeyFFT(64, 16, false, sfix16, sfix16, Seq(4, 4, 4)))
}
