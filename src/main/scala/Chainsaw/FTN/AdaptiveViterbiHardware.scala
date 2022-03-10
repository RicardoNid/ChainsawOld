package Chainsaw.FTN

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

/** parallel version of ViterbiHardware, using ViterbiHardware as cores
 * TODO: currently, trellis for 802.11 convenc is used, trellis should be set as a parameter
 */
case class AdaptiveViterbiHardware(parallelism: Int, parallelismForTest: Int = -1)
  extends Component with DSPTestable[Bits, Bits] {

  override val dataIn = slave Stream Bits(2 * parallelism bits)
  override val dataOut = master Stream Bits(parallelism bits)

  val actualParallelism = if (parallelismForTest == -1) parallelism else parallelismForTest

  // components
  val superInterleave = DSP.interleave.AdaptiveMatIntrlv(parallelism, 256, 2 * parallelism, 2 * parallelism, Bool)
  val superDeInterleave = DSP.interleave.AdaptiveMatIntrlv(128, parallelism, parallelism, parallelism, Bool)
  val vitdec = comm.viterbi.ViterbiHardware(
    trellis = trellis, blockLength = 128,
    copies = actualParallelism, readAsync = false, disWidth = 4)

  // transformation on payload
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

  // controls
  override val latency = Seq(superInterleave, superDeInterleave, vitdec).map(_.latency).sum + 1 // for m2s pipe
  //  val trigger = Trigger(dataIn.valid, 128)
  val valid = Delay(dataIn.valid, latency, init = False)
  val onceValid = RegNextWhen(True, dataIn.valid, init = False)
  val triggered = dataIn.valid || onceValid

  // connection
  dataIn.payloadMap(bits2bools).withValid(triggered) >> superInterleave.dataIn
  superInterleave.dataOut.payloadMap(bitRemapBeforeVit).payloadMap(cut).m2sPipe() >> vitdec.dataIn
  vitdec.dataOut.payloadMap(pad).payloadMap(uints2bools) >> superDeInterleave.dataIn
  superDeInterleave.dataOut.payloadMap(bools2bits).withValid(valid) >> dataOut
}
