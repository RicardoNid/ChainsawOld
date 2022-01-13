package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class RxFull(actual: Int, iteration: Int = iterationNum)
  extends RxLoop(Seq(0, 1, 2, 3, 4), actual)
    with DSPTestable[Vec[SInt], Bits] {

  val fdeType = HardType(Vec(symbolComplexType, 256))

  override val dataIn = slave Stream Vec(ADDAType, 256)
  override val dataOut = master Stream Bits(512 bits) // from vitdec
  override val latency = (5 + 1) * loopLength + 500 // this is just long enough for testing, no other meaning

  // components
  val front = RxFront()
  val equalizedBuffer = BigStreamFifo(fdeType, 2 * frameLength) // 32 = 2 * 16
  val fdeBuffer = FDEBuffer(fdeType, loopLength, frameLength, iteration)

  // dataIn -> rxFront
  dataIn >> front.dataIn
  front.dataOut >> equalizedBuffer.io.push

  // equalized buffer -> FDE buffer, burst transfer
  equalizedBuffer.io.pop >> fdeBuffer.dataIn
  fdeBuffer.start := equalizedBuffer.io.occupancy >= U(16)

  // construct diff
  val mapped: Vec[ComplexNumber] = doBitMask(qammod.dataOut.payload)
  val shortCutOutput: Vec[ComplexNumber] = BigDelay(mapped, 54)

  val deModulated = doBitMask(fftPost(fft.dataOut.payload))
  val fde = fdeBuffer.dataOut.payload

  val diff = Vec(fde.zip(deModulated.zip(shortCutOutput)).map { case (f, (a, b)) => f - (a - b) })

  shortCutOutput.simPublic()
  deModulated.simPublic()
  fde.simPublic()
  diff.simPublic()

  // FDE buffer -> Qamdemod
  val initTransfer = equalizedBuffer.io.pop.valid && fdeBuffer.dataIn.ready // fdeBuffer.dataIn.ready.fire, as fire can't be accessed outside
  qamdemod.dataIn.valid := RegNext(fdeBuffer.dataOut.valid || initTransfer, init = False)
  //  qamdemod.dataIn.payload := RegNext(Mux(fdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, fde)) // iter = 0
  qamdemod.dataIn.payload := RegNext(Mux(fdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, diff)) // iter = 4

  // Viterbi -> output
  vitdecs.dataOut.allowOverride
  val iterLastDelayed = Delay(fdeBuffer.iterLast, 516 + 19 + 1, init = False)
  iterLastDelayed.simPublic()
  vitdecs.dataOut
    .withValid(vitdecs.dataOut.valid && iterLastDelayed) >> dataOut
}
