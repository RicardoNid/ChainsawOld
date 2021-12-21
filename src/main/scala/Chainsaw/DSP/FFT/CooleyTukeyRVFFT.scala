package Chainsaw.DSP.FFT

import Chainsaw.dspTest.DSPTestable
import Chainsaw.{ComplexNumber, toComplexType}
import Chainsaw.DSP.{P2S, S2P}
import spinal.core.{Component, HardType, SFix, Vec}
import spinal.lib.{master, slave}

/** implement real-valued fft by doubling
 */
// TODO: different parallelism
case class CooleyTukeyRVFFT(N: Int, factors: Seq[Int],
                            dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component with DSPTestable[Vec[SFix], Vec[ComplexNumber]] {
  require(factors.last == 2)

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(dataType(), N)
  override val dataOut = master Stream Vec(complexType(), N)

  val pre = RVPreprocess(N, dataType)
  val p2s = P2S(N, N / 2, complexType)
  val core = CooleyTukeyBackToBack(N, N / 2, factors.init, Seq(2), false, dataType, coeffType)
  val s2p = S2P(N / 2, N, complexType)
  val post = RVPostprocess(N, dataType)

  override val latency = Seq(pre, p2s, core, s2p, post).map(_.latency).sum

  dataIn >> pre.dataIn
  pre.dataOut >> p2s.dataIn
  p2s.dataOut >> core.dataIn
  core.dataOut >> s2p.dataIn
  s2p.dataOut >> post.dataIn
  post.dataOut >> dataOut
}