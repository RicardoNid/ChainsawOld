package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

// TODO: Improve precision

/**
 * @param golden built-in symbols of 1/-1 for channel estimation
 */
case class EqualizerFTN(golden: Array[Int])(implicit ftnParams: FtnParams) extends
  Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  val smallVecType = HardType(Vec(smootherComplexType, 256 / 4))

  override val dataIn = slave Stream smootherComplexVecType()
  override val dataOut = master Stream equalizationComplexVecType()
  val splitter = SplitterFTN()
  val smooth = SmootherFTN(golden)
  val equal = EqualizationFTN()

  override val latency = smooth.latency + equal.latency + 2 // 2 for

  val preambleFIFO = BigStreamFifo(smootherComplexVecType(), 2)
  val dataFIFO = BigStreamFifo(equalizationComplexVecType, 32)
  val postPreambleFIFO = BigStreamFifo(equalizationComplexVecType(), 1)

  dataIn >> splitter.dataIn

  def trunc(in:Vec[ComplexNumber]) = Vec(in.map(_.truncated(equalizationType)))

  splitter.preambleOut >> preambleFIFO.io.push
  splitter.dataOut.payloadMap(trunc) >> dataFIFO.io.push

  // equal and smooth need burst transfer
  preambleFIFO.io.pop >> smooth.dataIn
  smooth.dataOut >> postPreambleFIFO.io.push

  postPreambleFIFO.io.pop >> equal.preambleIn
  dataFIFO.io.pop >> equal.dataIn

  def doBitMask(in: Vec[ComplexNumber]) =
    Vec(ftnParams.channelInfo.bitMask.zip(in).map { case (mask, data) => if (mask == 1) data else data.getZero })

  equal.dataOut.payloadMap(doBitMask) >> dataOut
}


