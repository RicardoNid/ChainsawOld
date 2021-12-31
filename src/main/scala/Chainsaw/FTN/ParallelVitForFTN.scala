package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class ParallelVitFTN(parallelism: Int, parallelismForTest: Int = -1)
  extends Component with DSPTestable[Bits, Bits] {

  override val dataIn = slave Stream Bits(2 * parallelism bits)
  override val dataOut = master Stream Bits(parallelism bits)

  val actualParallelism = if (parallelismForTest == -1) parallelism else parallelismForTest

  val superInterleave = DSP.interleave.AdaptiveMatIntrlv(parallelism, 256, 2 * parallelism, 2 * parallelism, Bool)
  val superDeInterleave = DSP.interleave.AdaptiveMatIntrlv(128, parallelism, parallelism, parallelism, Bool)
  val vitdec = comm.viterbi.ViterbiHardware(
    trellis = trellis, length = 128,
    copies = actualParallelism, readAsync = false, disWidth = 4)

  def cut(in: Vec[UInt]) = Vec(in.take(actualParallelism))

  def pad(in: Vec[UInt]) = Vec(in.padTo(parallelism, in.head.getZero))

  def bitRemapBeforeVit(in: Vec[Bool]) = Vec((0 until 2 * parallelism).map(i =>
    in(i % 2 * parallelism + i / 2))
    .grouped(2).toSeq.map(_.reverse.asBits().asUInt)
  )

  def bitRemapAfterConvenc(in: Vec[Bool]) = Vec((0 until 2 * parallelism).map(i =>
    in(i % parallelism * 2 + i / parallelism))
  )

  def uints2bools(in: Vec[UInt]) = Vec(in.map(_.asBool))

  dataIn.t(bits2bools) >> superInterleave.dataIn
  superInterleave.dataOut.t(bitRemapBeforeVit).t(cut) >> vitdec.dataIn
  vitdec.dataOut.t(pad).t(uints2bools) >> superDeInterleave.dataIn
  superDeInterleave.dataOut.t(bools2bits) >> dataOut

  override val latency = Seq(superInterleave, superDeInterleave, vitdec).map(_.latency).sum
}
