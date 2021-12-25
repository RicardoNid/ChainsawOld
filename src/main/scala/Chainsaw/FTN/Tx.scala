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

case class Tx(bitAlloc: Array[Int], powAlloc: Array[Double],
              qamPositions: Array[Int], qamRemapPositions: Array[Int])
  extends Component with DSPTestable[Bits, Vec[ComplexNumber]] {

  val unitType = HardType(SFix(2 exp, -15 exp))
  val unitComplexType = toComplexType(unitType)
  val dataType = HardType(SFix(7 exp, -8 exp))
  val complexType = toComplexType(dataType)

  val convenc = comm.channelCoding.Convenc128FTN()
  val interleave = DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool()))
  val s2p = DSP.S2P(256, 1024, HardType(Bool()))
  val qammod = comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType)
  val p2s = DSP.P2S(256, 64, unitComplexType)
  // TODO: using a hermitian symmetric module
  //  val ifft = DSP.FFT.CooleyTukeyBackToBack(512, 64, Seq(4, 4, 4), Seq(4, 2), inverse = true, dataType, dataType)

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

  def bitRemap(stream: Stream[Bits]) = {
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

  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val dataOut = master Stream Vec(toComplexType(unitType()), 64)
  override val latency = Seq(convenc, interleave, s2p, qammod, p2s).map(_.latency).sum

  dataIn >> convenc.dataIn
  bits2bool(convenc.dataOut) >> interleave.dataIn
  interleave.dataOut >> s2p.dataIn
  bitRemap(bools2bits(s2p.dataOut)) >> qammod.dataIn
  qammod.dataOut >> p2s.dataIn
  p2s.dataOut >> dataOut

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
