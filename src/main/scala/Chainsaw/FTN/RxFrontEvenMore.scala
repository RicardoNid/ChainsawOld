package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class RxFrontEvenMore(actual: Int)
  extends RxLoop(Seq(0, 1, 2, 3, 4), actual)
    with DSPTestable[Vec[SInt], Vec[ComplexNumber]] {

  val fdeType = HardType(Vec(symbolComplexType, 256))

  override val dataIn = slave Stream Vec(ADDAType, 256)
  override val dataOut = master Stream Vec(symbolComplexType, 256)
  override val latency = (5 + 1) * loopLength + 500 // this is just long enough for testing, no other meaning

  // components
  val front = RxFront()
  val equalizedBuffer = BigStreamFifo(fdeType, 2 * frameLength) // 32 = 2 * 16
  val fdeBuffer = FDEBuffer(fdeType, loopLength, frameLength, iterationNum)

  // dataIn -> rxFront
  dataIn >> front.dataIn
  front.dataOut >> equalizedBuffer.io.push

  // equalized buffer -> FDE buffer, burst transfer
  equalizedBuffer.io.pop >> fdeBuffer.dataIn
  fdeBuffer.start := equalizedBuffer.io.occupancy >= U(16)

  // FDE buffer -> Qamdemod
  val initTransfer = equalizedBuffer.io.pop.valid && fdeBuffer.dataIn.ready // fdeBuffer.dataIn.ready.fire, as fire can't be accessed outside
  qamdemod.dataIn.valid := RegNext(fdeBuffer.dataOut.valid || initTransfer, init = False)
  qamdemod.dataIn.payload := RegNext(Mux(fdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, fdeBuffer.dataOut.payload))

  fft.dataOut
    .payloadMap(fftPost)
    .withValid(fft.dataOut.valid && Delay(fdeBuffer.iterLast, loopLength - 1, init = False)) >> dataOut
}
