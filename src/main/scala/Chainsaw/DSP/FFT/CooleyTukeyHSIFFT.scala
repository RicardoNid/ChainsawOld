package Chainsaw.DSP.FFT

import Chainsaw.DSP.{P2S, S2P}
import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class CooleyTukeyHSIFFT(N: Int, pF: Int,
                             dataType: HardType[SFix], coeffType: HardType[SFix],
                             factors: Seq[Int], shifts: Seq[Int] = null)
                            (implicit complexMultConfig: ComplexMultConfig = ComplexMultConfig())
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[SFix]] {

  val fold = N / pF
  logger.info(s"implementing a $N-point hermitian symmetric ifft, folded by $fold")

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(complexType(), pF)

  // components
  val pre = HSPreprocess(N, dataType)
  val p2s0 = P2S(N, pF / 2, complexType)
  val core = AdaptiveCooleyTukeyFFT(N, pF / 2, true, dataType, coeffType, factors, shifts)

  val retDataType = core.retDataType
  val retComplexDataType = toComplexType(retDataType)

  val s2p1 = S2P(pF / 2, N, retComplexDataType)
  val post = HSPostprocess(N, retDataType)

  override val dataOut = master Stream Vec(retDataType, pF)

  var tempLatency = 0
  if (fold == 1) {
    dataIn >> pre.dataIn
    pre.dataOut >> p2s0.dataIn
    p2s0.dataOut >> core.dataIn
    core.dataOut >> s2p1.dataIn
    s2p1.dataOut >> post.dataIn
    post.dataOut >> dataOut
    tempLatency = Seq(p2s0, pre, core, s2p1, post).map(_.latency).sum
  }
  else {
    val s2p0 = S2P(pF, N, complexType)
    val p2s1 = P2S(N, pF, retDataType)
    val fifo = BigStreamFifo(Vec(retDataType, N), 2)

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

object CooleyTukeyHSIFFT {
  def main(args: Array[String]): Unit = {
    val dt = HardType(SFix(5 exp, -10 exp))
    val ct = HardType(SFix(1 exp, -14 exp))

    VivadoSynthForTiming(CooleyTukeyHSIFFT(64, 16, dt, ct, Seq(8, 4, 2)), "system_1_8")
    VivadoSynthForTiming(CooleyTukeyHSIFFT(64, 32, dt, ct, Seq(8, 2, 4)), "system_1_4")
    VivadoSynthForTiming(CooleyTukeyHSIFFT(64, 64, dt, ct, Seq(8, 4, 2)), "system_1_2")
    VivadoSynthForTiming(CooleyTukeyFFT(64, true, dt, ct, Seq(8, 8)), "system_1_1")
  }
}