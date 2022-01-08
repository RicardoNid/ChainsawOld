package Chainsaw.DSP.FFT

import Chainsaw.dspTest.DSPTestable
import Chainsaw.{ComplexNumber, toComplexType}
import Chainsaw.DSP.{P2S, S2P}
import spinal.core.{Component, HardType, SFix, Vec}
import spinal.lib.{master, slave}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

/** implement real-valued fft by doubling
 */
// TODO: different parallelism
case class CooleyTukeyRVFFT(N: Int, pF: Int,
                            dataType: HardType[SFix], coeffType: HardType[SFix],
                            factors: Seq[Int], shifts: Seq[Int] = null)
  extends Component with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {

  val fold = N / pF
  logger.info(s"implementing a $N-point real valued fft, folded by $fold")

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(dataType(), pF)

  val pre = RVPreprocess(N, dataType)
  val p2s0 = P2S(N, pF / 2, complexType)
  val core = AdaptiveCooleyTukeyFFT(N, pF / 2, false, dataType, coeffType, factors, shifts)

  val retDataType = core.retDataType
  val retComplexDataType = toComplexType(retDataType)

  val s2p1 = S2P(pF / 2, N, retComplexDataType)
  val post = RVPostprocess(N, retDataType)

  override val dataOut = master Stream Vec(retComplexDataType, pF)

  var tempLatency = 0
  if (fold == 1) {
    dataIn >> pre.dataIn
    pre.dataOut >> p2s0.dataIn
    p2s0.dataOut >> core.dataIn
    core.dataOut >> s2p1.dataIn
    s2p1.dataOut >> post.dataIn
    post.dataOut >> dataOut
    tempLatency = Seq(p2s0, pre, core, s2p1, post).map(_.latency).sum
  } else {
    val s2p0 = S2P(pF, N, dataType)
    val p2s1 = P2S(N, pF, retComplexDataType)
    val fifo = BigStreamFifo(Vec(retComplexDataType, N), 2)

    dataIn >> s2p0.dataIn
    s2p0.dataOut >> pre.dataIn
    pre.dataOut >> p2s0.dataIn
    p2s0.dataOut >> core.dataIn
    core.dataOut >> s2p1.dataIn
    s2p1.dataOut >> post.dataIn
    post.dataOut >> fifo.io.push
    fifo.io.pop >> p2s1.dataIn
    p2s1.dataOut >> dataOut
    tempLatency = Seq(s2p0, pre, p2s0, core, s2p1, post, p2s1).map(_.latency).sum + (fold - 1) + 2 // 2 for fifo, extra fold - 1 for pre
  }

  override val latency = tempLatency
}