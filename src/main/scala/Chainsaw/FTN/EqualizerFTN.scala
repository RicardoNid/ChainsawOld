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

  // to avoid "way too big signal"
  val smallVecType = HardType(Vec(equalizerComplexType, 256 / 4))

  override val dataIn = slave Stream equalizerComplexVecType()
  override val dataOut = master Stream equalizerComplexVecType()
  val splitter = SplitterFTN()

  val smooth = SmootherFTN(golden)
  val equal = EqualizationFTN()

  override val latency = smooth.latency + equal.latency

  val preambleFIFOs = (0 until 4).map(_ => StreamFifo(smallVecType(), 1))
  val dataFIFOs = (0 until 4).map(_ => StreamFifo(smallVecType(), 16))
  val postPreambleFIFOs = (0 until 4).map(_ => StreamFifo(smallVecType(), 1))

  dataIn >> splitter.dataIn

  def one2four(in: Stream[Vec[ComplexNumber]], outs: Seq[StreamFifo[Vec[ComplexNumber]]]) = {
    outs.zipWithIndex.foreach { case (out, i) =>
      out.io.push.valid := in.valid
      out.io.push.payload := Vec(in.payload.slice(i * equalizerWidth / 4, (i + 1) * equalizerWidth / 4))
    }
    in.ready := outs.map(_.io.push.ready).reduce(_ && _)
  }

  def four2one(ins: Seq[StreamFifo[Vec[ComplexNumber]]], out: Stream[Vec[ComplexNumber]]) = {
    ins.zipWithIndex.foreach { case (in, i) =>
      out.payload.slice(i * equalizerWidth / 4, (i + 1) * equalizerWidth / 4).zip(in.io.pop.payload).foreach { case (outData, inData) => outData := inData }
      in.io.pop.ready := out.ready
    }
    out.valid := ins.map(_.io.pop.valid).reduce(_ && _)
  }

  one2four(splitter.preambleOut, preambleFIFOs)
  one2four(splitter.dataOut, dataFIFOs)

  four2one(preambleFIFOs, smooth.dataIn)
  one2four(smooth.dataOut, postPreambleFIFOs)

  four2one(postPreambleFIFOs, equal.preambleIn)
  four2one(dataFIFOs, equal.dataIn)

  def doBitMask(in: Vec[ComplexNumber]) =
    Vec(channelInfo.bitMask.zip(in).map { case (mask, data) => if (mask == 1) data else data.getZero })

  equal.dataOut.t(doBitMask) >> dataOut
}


