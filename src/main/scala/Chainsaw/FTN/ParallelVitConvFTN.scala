package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

/** this module do parallel vitdec and convenc for FTN
 *
 * @param parallelism we provide this option for smaller test
 */
case class ParallelVitConvFTN(parallelism: Int, parallelismForTest: Int = -1)
  extends Component with DSPTestable[Bits, Bits] {

  override val dataIn = slave Stream Bits(2 * parallelism bits)
  override val dataOut = master Stream Bits(2 * parallelism bits)

  val actualParallelism = if(parallelismForTest == -1) parallelism else parallelismForTest

  val superInterleave = DSP.interleave.AdaptiveMatIntrlv(parallelism, 256, 2 * parallelism, 2 * parallelism, Bool)
  val superDeInterleave = DSP.interleave.AdaptiveMatIntrlv(256, parallelism, 2 * parallelism, 2 * parallelism, Bool)
  val vitdec = comm.viterbi.ViterbiHardware(trellis, length = 128, copies = actualParallelism, readAsync = false, disWidth = 8)
  val convenc = comm.channelCoding.ConvencFTNRx(actualParallelism)

  def cut(in: Vec[UInt]) = Vec(in.take(actualParallelism))

  def pad(in: Vec[UInt]) = Vec(in.padTo(parallelism, in.head.getZero))

  def bitRemapBeforeVit(in: Vec[Bool]) = Vec((0 until 2 * parallelism).map(i =>
    in(i % 2 * parallelism + i / 2))
    .grouped(2).toSeq.map(_.reverse.asBits().asUInt)
  )

  def bitRemapAfterConvenc(in: Vec[Bool]) = Vec((0 until 2 * parallelism).map(i =>
    in(i % parallelism * 2 + i / parallelism))
  )

  def uints2bools(in: Vec[UInt]) = Vec(in.reverse.asBits().asBools.reverse)

  dataIn.t(bits2bools) >> superInterleave.dataIn
  superInterleave.dataOut.t(bitRemapBeforeVit).t(cut) >> vitdec.dataIn
  vitdec.dataOut >> convenc.dataIn
  convenc.dataOut.t(pad).t(uints2bools).t(bitRemapAfterConvenc) >> superDeInterleave.dataIn
  superDeInterleave.dataOut.t(bools2bits) >> dataOut

  override val latency = Seq(superInterleave, superDeInterleave, vitdec, convenc).map(_.latency).sum
}


