package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class Tx(bitAlloc: Array[Int], powAlloc: Array[Double], bitMask: Array[Int],
              qamPositions: Array[Int], qamRemapPositions: Array[Int])
  extends Component with DSPTestable[Bits, Vec[SFix]] {

  val unitType = HardType(SFix(2 exp, -15 exp))
  val unitComplexType = toComplexType(unitType)
  val dataType = HardType(SFix(7 exp, -15 exp))
  val complexType = toComplexType(dataType)

  val convenc = comm.channelCoding.Convenc128FTN()
  val interleave = DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool()))
  val s2p = DSP.S2P(256, 1024, HardType(Bool()))
  val qammod = comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType)
  val p2s = DSP.P2S(512, 128, complexType)
  val ifft = DSP.FFT.CooleyTukeyBackToBack(512, 128, Seq(4, 4, 4, 2), Seq(4), true, dataType, unitType)


  def bools2bits(stream: Stream[Vec[Bool]]) = {
    val ret = Stream(Bits(stream.payload.length bits))
    ret.payload := stream.payload.reverse.asBits()
    ret.valid := stream.valid
    stream.ready := ret.valid
    ret
  }

  def bits2bool(stream: Stream[Bits]) = {
    val ret = Stream(Vec(Bool(), stream.payload.getBitsWidth))
    ret.payload.zip(stream.payload.asBools.reverse).foreach { case (a, b) => a := b }
    ret.valid := stream.valid
    stream.ready := ret.valid
    ret
  }

  def doBitRemap(stream: Stream[Bits]) = {
    val before = stream.payload.asBools.reverse
    val remapped = (0 until 1024).map { i =>
      val index = qamRemapPositions.indexOf(i)
      if (index == -1) False else before(qamPositions(index))
    }.reverse.asBits()
    val ret = Stream(Bits(1024 bits))
    ret.payload := remapped
    ret.valid := stream.valid
    stream.ready := ret.valid
    ret
  }

  def doBitMask(stream: Stream[Vec[ComplexNumber]]) = {
    val masked = stream.payload.zip(bitMask).map { case (data, mask) => if (mask == 1) data else stream.payload.head.getZero }
    val ret = Stream(Vec(unitComplexType(), 256))
    ret.payload := Vec(masked)
    ret.valid := stream.valid
    stream.ready := ret.valid
    ret
  }

  def ifftPre(stream: Stream[Vec[ComplexNumber]]) = {
    val before = stream.payload
    val zero = complexType().getZero
    val hs = (0 until 512).map {
      case 0 => zero
      case 256 => zero
      case i => if (i < 256) before(i).truncated(dataType) else before(512 - i).conj.truncated(dataType)
    }
    val ret = Stream(cloneOf(Vec(hs)))
    ret.payload := Vec(hs)
    ret.valid := stream.valid
    stream.ready := ret.valid
    ret
  }

  def ifftPost(stream: Stream[Vec[ComplexNumber]]) = {
    val before = stream.payload
    val ret = Stream(Vec(dataType(), 128))
    // FIXME: it is not necessary for real implementation to use zero
    ret.payload := Vec(before.map(_.real))
    ret.valid := stream.valid
    stream.ready := ret.valid
    ret
  }

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(dataType(), 128)
  // FIXME: part of the latency is wrong
  override val latency = Seq(convenc, interleave, s2p, qammod, p2s, ifft).map(_.latency).sum + 20

  dataIn >> convenc.dataIn
  bits2bool(convenc.dataOut) >> interleave.dataIn
  interleave.dataOut >> s2p.dataIn
  doBitRemap(bools2bits(s2p.dataOut)) >> qammod.dataIn
  ifftPre(doBitMask(qammod.dataOut)) >> p2s.dataIn
  p2s.dataOut >> ifft.dataIn
  ifftPost(ifft.dataOut) >> dataOut

  logger.info(s"Tx generated, latency = $latency")

  // for sim
  val interleaveOut = interleave.dataOut.payload.reverse.asBits()
  val s2pIn = s2p.dataIn.payload.reverse.asBits()
  val s2pOut = s2p.dataOut.payload.reverse.asBits()
  Seq(interleaveOut, s2pIn, s2pOut).foreach(_.simPublic())
}

object Tx {
  def main(args: Array[String]): Unit = {
    //    GenRTL(Tx())
  }
}
