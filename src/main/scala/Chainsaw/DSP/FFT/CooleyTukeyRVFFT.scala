package Chainsaw.DSP.FFT

import Chainsaw.dspTest.DSPTestable
import Chainsaw.{ComplexNumber, toComplexType}
import Chainsaw.DSP.{P2S, S2P}
import spinal.core.{Component, HardType, SFix, Vec}
import spinal.lib.{master, slave}

/** implement real-valued fft by doubling
 */
// TODO: different parallelism
case class CooleyTukeyRVFFT(N: Int, factors1: Seq[Int], factors2: Seq[Int],
                            dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {

  val pF = factors1.product * 2
  require(N % pF == 0)

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(dataType(), pF)
  override val dataOut = master Stream Vec(complexType(), pF)

  val pre = RVPreprocess(N, dataType)
  val p2s0 = P2S(N, pF / 2, complexType)
  val core = CooleyTukeyBackToBack(N, pF / 2, factors1, factors2, false, dataType, coeffType)
  val s2p1 = S2P(pF / 2, N, complexType)
  val post = RVPostprocess(N, dataType)

  override val latency = Seq(pre, p2s0, core, s2p1, post).map(_.latency).sum

  if (pF == N) {
    dataIn >> pre.dataIn
    pre.dataOut >> p2s0.dataIn
    p2s0.dataOut >> core.dataIn
    core.dataOut >> s2p1.dataIn
    s2p1.dataOut >> post.dataIn
    post.dataOut >> dataOut
  } else {
    throw new IllegalArgumentException("folded RVFFT is not prepared as pre & post are not designed for folded situation")
    //    val s2p0 = S2P(pF, N, dataType)
    //    val p2s1 = P2S(N, pF, complexType)
    //
    //    dataIn >> s2p0.dataIn
    //    s2p0.dataOut >> pre.dataIn
    //    pre.dataOut >> p2s0.dataIn
    //    p2s0.dataOut >> core.dataIn
    //    core.dataOut >> s2p1.dataIn
    //    s2p1.dataOut >> post.dataIn
    //    post.dataOut >> p2s1.dataIn
    //    p2s1.dataOut >> dataOut
  }
}