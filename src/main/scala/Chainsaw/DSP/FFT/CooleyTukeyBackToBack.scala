package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class CooleyTukeyBackToBack(
                                  N: Int = 256, pF: Int = 32,
                                  factors1: Seq[Int] = Seq(4, 4, 2), factors2: Seq[Int] = Seq(4, 2),
                                  inverse: Boolean = false,
                                  dataType: HardType[SFix], coeffType: HardType[SFix],
                                  shifts1: Seq[Int] = null, shifts2: Seq[Int] = null)
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  val complexDataType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(complexDataType, pF)

  val N1 = pF
  val N2 = N / pF

  // components
  val core0 = CooleyTukeyFFT(N1, factors1, inverse, dataType, coeffType, shifts1)
  val interDataType = core0.retDataType
  val interComplexDataType = toComplexType(interDataType)
  val core1s = Seq.fill(N1 / N2)(CooleyTukeyFFT(N2, factors2, inverse, interDataType, coeffType, shifts2))
  val retDataType = core1s.head.retDataType
  val retComplexDataType = toComplexType(retDataType)

  val inter0 = DSP.interleave.AdaptiveMatIntrlv(N1, N2, pF, pF, complexDataType)
  val inter1 = DSP.interleave.AdaptiveMatIntrlv(N2, N1, pF, pF, interComplexDataType)
  val inter2 = DSP.interleave.AdaptiveMatIntrlv(N1, N2, pF, pF, retComplexDataType)

  override val dataOut = master Stream Vec(retComplexDataType, pF)

  override val latency = Seq(inter0, inter1, inter2, core0, core1s.head).map(_.latency).sum + cmultConfig.pipeline
  logger.info(s"implementing a $N-point folded ${if (inverse) "ifft" else "fft"} module at parallel factor = $pF, latency = $latency")

  //  val twiddleFactors: Seq[Seq[MComplex]] = Algos.cooleyTukeyCoeffIndices(N1, N2).map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
  val twiddleFactors: Seq[Seq[BComplex]] = Algos.cooleyTukeyCoeffIndices(N2, N1).map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
  //  val twiddleFactorROM: Mem[Vec[ComplexNumber]] = Mem(twiddleFactors.map(vec => Vec(vec.map(CN(_, coeffType)))))
  val twiddleFactorROMs = twiddleFactors.map(vec => Mem(Vec(vec.map(CN(_, coeffType)))))
  val factorCounter = Counter(N2, inc = core0.dataOut.fire)
  logger.info(s"twiddle factors num: $N1 * $N2")
  //  val currentFactors: Vec[ComplexNumber] = twiddleFactorROM.readAsync(factorCounter.value)
  val currentFactors: Vec[ComplexNumber] = Vec(twiddleFactorROMs.map(_.readAsync(factorCounter.value)))

  dataIn >> inter0.dataIn // outer -> inter0
  inter0.dataOut.toFlow >> core0.dataIn // inter0 -> core1

  // core1 -> multiply twiddle factor -> inter1
  inter1.dataIn.valid := Delay(core0.dataOut.valid, cmultConfig.pipeline, init = False)
  val multiplied: Vec[ComplexNumber] = Vec(core0.dataOut.payload
    .zip(currentFactors)
    .map { case (data, coeff) => (data * coeff).truncated(interDataType) })
  inter1.dataIn.payload := multiplied

  // inter1 -> core2s
  core1s.zip(inter1.dataOut.payload.grouped(N2).toSeq).foreach { case (core, fixes) => core.dataIn.payload := Vec(fixes) }
  core1s.foreach(_.dataIn.valid := inter1.dataOut.valid)
  inter1.dataOut.ready := True
  // core2s -> inter2
  inter2.dataIn.payload := Vec(core1s.map(_.dataOut.payload.toSeq).reduce(_ ++ _))
  inter2.dataIn.valid := core1s.head.dataOut.valid
  inter2.dataOut >> dataOut // inter2 -> outer
}
