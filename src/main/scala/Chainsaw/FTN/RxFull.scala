package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class RxFull(actual: Int, iteration: Int)(implicit ftnParams: FtnParams)
  extends RxLoop(Seq(0, 1, 2, 3, 4), actual)
    with DSPTestable[Vec[SInt], Bits] {

  val fdeType = HardType(Vec(symbolComplexType, 256))

  override val dataIn = slave Stream Vec(ADDAType, 128)
  override val dataOut = master Stream Bits(512 bits) // from vitdec

  // calculation
  val loopLatency = qamdemod.latency + deInterleave.latency +
    vitdecs.latency + convencs.latency +
    interleave.latency + qammod.latency +
    ifft.latency + fft.latency +
    1 // for diff
  val extraLatency = 8 * 16 - (loopLatency % (8 * 16))
  //  val extraLatency = 0
  logger.info(s"latency starts from qamdemod = ${qamdemod.latency}, ${deInterleave.latency}, " +
    s"${vitdecs.latency}, ${convencs.latency}, " +
    s"${interleave.latency}, ${qammod.latency}, " +
    s"${ifft.latency},${fft.latency}")
  logger.info(s"loop latency = $loopLatency")
  logger.info(s"extra latency = $extraLatency")
  val fullLoopLatency = loopLatency + extraLatency
  override val latency = (5 + 1) * fullLoopLatency + 500 // this is just long enough for testing, no other meaning

  // components
  // besides the components below, this module inherit sub-components from RxLoop
  val front = RxFront()
  val equalizedBuffer = BigStreamFifo(fdeType, 2 * ftnParams.frameLengthInRx) // 32 = 2 * 16
  val fdeBuffer = FDEBuffer(fdeType, fullLoopLatency, ftnParams.frameLengthInRx, iteration)

  // connections
  // dataIn -> rxFront
  dataIn >> front.dataIn
  front.dataOut >> equalizedBuffer.io.push

  // equalized buffer -> FDE buffer, burst transfer
  equalizedBuffer.io.pop >> fdeBuffer.dataIn
  fdeBuffer.start := equalizedBuffer.io.occupancy >= U(16)

  // construct diff
  val mapped: Vec[ComplexNumber] = doBitMask(qammod.dataOut.payload)
  // 3 operands for diff
  val shortCutOutput: Vec[ComplexNumber] = BigDelay(mapped, ifft.latency + fft.latency)

  val deModulated = doBitMask(fftPostInLoop(fft.dataOut.payload))
  val fde = fdeBuffer.dataOut.payload
  // diff
  val diff0 = Vec(deModulated.zip(shortCutOutput).map { case (a, b) => a - b }) //
  val diff0Delayed = if (extraLatency == 0) diff0 // to pad latency to a more regular number
  else BigDelay(diff0, extraLatency)
  val diff1 = Vec(fde.zip(diff0Delayed).map { case (a, b) => a - b })

  // show diffs
  shortCutOutput.simPublic()
  deModulated.simPublic()
  fde.simPublic()
  diff0.simPublic()
  diff1.simPublic()

  // FDE buffer -> Qamdemod
  val initTransfer = equalizedBuffer.io.pop.valid && fdeBuffer.dataIn.ready // fdeBuffer.dataIn.ready.fire, as fire can't be accessed outside
  qamdemod.dataIn.valid := RegNext(fdeBuffer.dataOut.valid || initTransfer, init = False)

  if (iteration == 1) qamdemod.dataIn.payload := RegNext(Mux(fdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, fde)) // no iteration
  else qamdemod.dataIn.payload := RegNext(Mux(fdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, diff1))

  // Viterbi -> output
  vitdecs.dataOut.allowOverride
  val delayToVitdec =  1 + qamdemod.latency + deInterleave.latency + vitdecs.latency // 1 for diff
  val iterLastDelayed = Delay(fdeBuffer.iterLast, delayToVitdec, init = False)
  iterLastDelayed.simPublic()
  vitdecs.dataOut.withValid(vitdecs.dataOut.valid && iterLastDelayed) >> dataOut
}
