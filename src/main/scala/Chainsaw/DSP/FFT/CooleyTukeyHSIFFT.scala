package Chainsaw.DSP.FFT

import Chainsaw.DSP.{P2S, S2P}
import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class CooleyTukeyHSIFFT(N: Int, factors: Seq[Int],
                               dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[SFix]] {
  require(factors.last == 2)

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(complexType(), N)
  override val dataOut = master Stream Vec(dataType(), N)

  val pre = HSPreprocess(N, dataType)
  val p2s = P2S(N, N / 2, complexType)
  val core = CooleyTukeyBackToBack(N, N / 2, factors.init, Seq(2), true, dataType, coeffType)
  val s2p = S2P(N / 2, N, complexType)
  val post = HSPostprocess(N, dataType)

  override val latency = Seq(pre, p2s, core, s2p, post).map(_.latency).sum

  dataIn >> pre.dataIn
  pre.dataOut >> p2s.dataIn
  p2s.dataOut >> core.dataIn
  core.dataOut >> s2p.dataIn
  s2p.dataOut >> post.dataIn
  post.dataOut >> dataOut

}
