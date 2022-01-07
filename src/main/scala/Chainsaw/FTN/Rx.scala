package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class Rx(actual: Int)
  extends RxLoop(Seq(0, 1), actual)
    with DSPTestable[Vec[SFix], Bits] {

  val dataInType = HardType(Vec(fftType, 128))
  val fdeType = HardType(Vec(unitComplexType, 256))
  val loopLength = 608
  val frameLength = 16
  val iteration = 5

  override val dataIn = slave Stream dataInType
  override val dataOut = master Stream Bits(512 bits)
  override val latency = (5 + 1) * loopLength + 500

  // components
  val front = RxFront()
  val equalizedBuffer = BigStreamFifo(fdeType, 2 * frameLength) // 32 = 2 * 16
  val FdeBuffer = LoopBuffer(fdeType, loopLength, frameLength, iteration)

  dataIn >> front.dataIn
  front.dataOut >> equalizedBuffer.io.push

  // equalized buffer -> FDE buffer, burst transfer
  equalizedBuffer.io.pop >> FdeBuffer.dataIn
  FdeBuffer.start := equalizedBuffer.io.occupancy >= U(16)

  // FDE buffer -> Qamdemod
  FdeBuffer.dataOut >> qamdemod.dataIn
  qamdemod.dataIn.allowOverride
  qamdemod.dataIn.payload := Mux(FdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, FdeBuffer.dataOut.payload)

  // Viterbi -> output
  vitdecs.dataOut >> dataOut
  dataOut.allowOverride
  dataOut.valid := Delay(FdeBuffer.iterLast, 516 + 19, init = False)
}

case class RxFull(actual: Int)
  extends RxLoop(Seq(0, 1, 2, 3, 4), actual)
    with DSPTestable[Vec[SFix], Bits] {

  val dataInType = HardType(Vec(fftType, 128))
  val fdeType = HardType(Vec(unitComplexType, 256))
  val loopLength = 608
  val frameLength = 16
  val iteration = 5

  override val dataIn = slave Stream dataInType
  override val dataOut = master Stream Bits(512 bits)
  override val latency = (5 + 1) * loopLength + 500

  // components
  val front = RxFront()
  val equalizedBuffer = BigStreamFifo(fdeType, 2 * frameLength) // 32 = 2 * 16
  val FdeBuffer = LoopBuffer(fdeType, loopLength, frameLength, iteration)

  dataIn >> front.dataIn
  front.dataOut >> equalizedBuffer.io.push

  // equalized buffer -> FDE buffer, burst transfer
  equalizedBuffer.io.pop >> FdeBuffer.dataIn
  FdeBuffer.start := equalizedBuffer.io.occupancy >= U(16)

  // shortcut of mapped symbols
  val mapped: Vec[ComplexNumber] = qammod.dataOut.payload
  val shortCut = BigMem(HardType(mapped), 54)
  val shortCutCounter = CounterFreeRun(54)
  shortCut.write(shortCutCounter, mapped)
  val shortCutOutput = shortCut.readSync(shortCutCounter.valueNext)

  val deModulated = fft.dataOut.payload

  def getDiff(fde: Vec[ComplexNumber]) = {
    Vec(fde.zip(deModulated.zip(shortCutOutput)).map { case (f, (a, b)) =>
      val ici = a - b
      RegNext(f - ici)
    })
  }

  // FDE buffer -> Qamdemod
  FdeBuffer.dataOut >> qamdemod.dataIn
  qamdemod.dataIn.allowOverride
  qamdemod.dataIn.payload := Mux(FdeBuffer.dataIn.ready, equalizedBuffer.io.pop.payload, getDiff(FdeBuffer.dataOut.payload))

  // Viterbi -> output
  vitdecs.dataOut.allowOverride
  vitdecs.dataOut >> dataOut
  dataOut.allowOverride
  dataOut.valid := Delay(FdeBuffer.iterLast, 516 + 19, init = False)
}