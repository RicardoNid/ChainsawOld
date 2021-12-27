package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class Tx(bitAlloc: Array[Int], powAlloc: Array[Double], bitMask: Array[Int],
              qamPositions: Array[Int], qamRemapPositions: Array[Int])
  extends Component with DSPTestable[Bits, Vec[SFix]] {

  val unitType = HardType(SFix(2 exp, -15 exp))
  val unitComplexType = toComplexType(unitType)
  val dataType = HardType(SFix(8 exp, -15 exp))
  val complexType = toComplexType(dataType)

  implicit class StreamUtil[Ti <: Data](stream: Stream[Ti]) {
    def t[To <: Data](transform: Ti => To) = {
      val ret = transform(stream.payload)
      val retStream = Stream(cloneOf(ret))
      retStream.payload := ret
      retStream.valid := stream.valid
      stream.ready := retStream.valid
      retStream
    }
  }

  val convenc = comm.channelCoding.Convenc128FTN()
  val interleave = DSP.interleave.AdaptiveMatIntrlv(256, 64, 256, 256, HardType(Bool()))
  val s2p = DSP.S2P(256, 1024, HardType(Bool()))
  val qammod = comm.qam.AdaptiveQammod(bitAlloc, powAlloc, unitType)
  val p2s = DSP.P2S(512, 128, complexType)
  val ifft = DSP.FFT.CooleyTukeyBackToBack(512, 128, Seq.fill(7)(2), Seq(2, 2), true, dataType, unitType)

  // FIXME: part of the latency is wrong
  override val dataIn = slave(cloneOf(convenc.dataIn))
  override val latency = Seq(convenc, interleave, s2p, qammod, p2s, ifft).map(_.latency).sum + 20
  override val dataOut = master Stream Vec(dataType(), 128)

  dataIn >> convenc.dataIn
  convenc.dataOut.t(bits2bools) >> interleave.dataIn
  interleave.dataOut >> s2p.dataIn
  s2p.dataOut.t(bools2bits).t(bitRemap) >> qammod.dataIn
  qammod.dataOut.t(doBitMask).t(ifftPre) >> p2s.dataIn
  p2s.dataOut >> ifft.dataIn
  ifft.dataOut.t(ifftPost) >> dataOut

  logger.info(s"Tx generated, latency = $latency")

  def complexZero = complexType().getZero

  def bools2bits(in: Vec[Bool]) = in.reverse.asBits()

  def bits2bools(in: Bits) = Vec(in.asBools.reverse)

  def bitRemap(in: Bits) = {
    val bools = in.asBools.reverse
    (0 until 1024).map { i =>
      val index = qamRemapPositions.indexOf(i)
      if (index == -1) False else bools(qamPositions(index))
    }.reverse.asBits()
  }

  def doBitMask(in: Vec[ComplexNumber]) = Vec(in.zip(bitMask).map { case (data, mask) => if (mask == 1) data else complexZero })

  def ifftPre(in: Vec[ComplexNumber]) = Vec((0 until 512).map {
    case 0 => complexZero
    case 256 => complexZero
    case i => if (i < 256) in(i).truncated(dataType) else in(512 - i).conj.truncated(dataType)
  })

  def ifftPost(in: Vec[ComplexNumber]) = Vec(in.map(_.real))
}