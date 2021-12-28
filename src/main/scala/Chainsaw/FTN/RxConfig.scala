package Chainsaw.FTN

import spinal.core._

object RxConfig {

  val fftSize = 256
  val fftDataType = HardType(SFix(7 exp, 18 bits))

  val testType = HardType(UInt(5 bits))

  val smoothBatch = 2
  val smoothLatency = 21

  val equalBatch0 = 1
  val equalBatch1 = 16
  val equalLatency0 = 30
  val equalLatency1 = 48

  val ifftFftLatency = 30
  val othersInLoopLatency = 130
  val loopLatency = ifftFftLatency + othersInLoopLatency
}
