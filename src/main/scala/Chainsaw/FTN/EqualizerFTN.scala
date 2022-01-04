package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

// TODO: Improve precision

/**
 * @param golden built-in symbols of 1/-1 for channel estimation
 * @param unitType
 * @param vecSize
 */
case class EqualizerFTN(golden: Array[Int]) extends
  Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  val smallVecType = HardType(Vec(equalizerComplexType, 256 / 4))

  override val dataIn = slave Stream equalizerComplexVecType()
  override val dataOut = master Stream equalizerComplexVecType()
  val splitter = SplitterFTN()

  val smooth = SmootherFTN(golden)
  val equal = EqualizationFTN()

  override val latency = smooth.latency + equal.latency + 2 // 2 for

  val preambleFIFO = BigStreamFifo(equalizerComplexVecType(), 2)
  val dataFIFO = BigStreamFifo(equalizerComplexVecType(), 32)
  val postPreambleFIFO = BigStreamFifo(equalizerComplexVecType(), 1)

  dataIn >> splitter.dataIn

  splitter.preambleOut >> preambleFIFO.io.push
  splitter.dataOut >> dataFIFO.io.push

  // equal and smooth need burst transfer
  preambleFIFO.io.pop >> smooth.dataIn
  smooth.dataOut >> postPreambleFIFO.io.push

  postPreambleFIFO.io.pop >> equal.preambleIn
  dataFIFO.io.pop >> equal.dataIn

  def doBitMask(in: Vec[ComplexNumber]) =
    Vec(channelInfo.bitMask.zip(in).map { case (mask, data) => if (mask == 1) data else data.getZero })

  equal.dataOut.t(doBitMask) >> dataOut
}


