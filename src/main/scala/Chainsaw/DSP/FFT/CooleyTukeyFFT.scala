package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

/** implement fft hardware by CooleyTukey indexing method, the output is digit-reversed
 *
 * @param N         length of fft/ifft
 * @param factors   factors for decomposition, for each factor, the corresponding DFT algo must exist
 * @param inverse   fft/ifft
 * @param dataType  hard type for datapath
 * @param coeffType hard type for coeffs
 * @see ''Fast Algorithms for Signal Processing'' Chap3.1
 */
@coreHardware
@fullyPipelined
case class CooleyTukeyFFT(N: Int = 256, factors: Seq[Int], inverse: Boolean = false,
                          dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  logger.info(s"implementing a $N-point fully pipelined ${if (inverse) "ifft" else "fft"} module")

  val complexDataType = toComplexType(dataType)
  override val dataIn = slave Flow Vec(complexDataType(), N)
  override val dataOut = master Flow Vec(complexDataType(), N)

  val getDft: Int => DFT = DFT(_, inverse, dataType, coeffType)

  // using DFT as building blocks
  def block(input: Seq[ComplexNumber]): Seq[ComplexNumber] = {
    val dft = getDft(input.size)
    dft.dataIn := Vec(input)
    dft.dataOut
  }

  // using complex multipliers
  val complexMultLatency = 3
  val complexMult: (ComplexNumber, Int, Int) => ComplexNumber = multiplyWNnk(_, _, _, dataType, coeffType)(complexMultLatency)

  // define latency
  override val latency = factors.map(getDft(_).latency).sum + (factors.length - 1) * complexMultLatency

  // wrapper
  dataOut.payload := Vec(Algos.cooleyTukeyBuilder(dataIn.payload, factors, block, complexMult, inverse))
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}

case class CooleyTukeyBackToBack(
                                  N: Int = 256, pF: Int = 32,
                                  factors1: Seq[Int] = Seq(4, 4, 2), factors2: Seq[Int] = Seq(4, 2),
                                  inverse: Boolean = false,
                                  dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  logger.info(s"implementing a $N-point folded ${if (inverse) "ifft" else "fft"} module at parallel factor = $pF")

  val complexDataType = toComplexType(dataType)
  override val dataIn = slave Stream Vec(complexDataType, pF)
  override val dataOut = master Stream Vec(complexDataType, pF)

  val N1 = pF
  val N2 = N / pF

  // components
  val core1 = CooleyTukeyFFT(N1, factors1, inverse, dataType, coeffType)
  val core2s = Seq.fill(N1 / N2)(CooleyTukeyFFT(N2, factors2, inverse, dataType, coeffType))
  val inter0 = DSP.interleave.AdaptiveMatIntrlv(N1, N2, pF, pF, complexDataType)
  val inter1 = DSP.interleave.AdaptiveMatIntrlv(N2, N1, pF, pF, complexDataType)
  val inter2 = DSP.interleave.AdaptiveMatIntrlv(N1, N2, pF, pF, complexDataType)

  override val latency = inter0.latency + core1.latency + inter1.latency + core2s.head.latency + +inter2.latency

  //  val twiddleFactors: Seq[Seq[MComplex]] = Algos.cooleyTukeyCoeffIndices(N1, N2).map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
  val twiddleFactors: Seq[Seq[BComplex]] = Algos.cooleyTukeyCoeffIndices(N2, N1).map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
  //  val twiddleFactorROM: Mem[Vec[ComplexNumber]] = Mem(twiddleFactors.map(vec => Vec(vec.map(CN(_, coeffType)))))
  val twiddleFactorROMs = twiddleFactors.map(vec => Mem(Vec(vec.map(CN(_, coeffType)))))
  val factorCounter = Counter(N2, inc = core1.dataOut.fire)
  logger.info(s"twiddle factors num: $N1 * $N2")
  //  val currentFactors: Vec[ComplexNumber] = twiddleFactorROM.readAsync(factorCounter.value)
  val currentFactors: Vec[ComplexNumber] = Vec(twiddleFactorROMs.map(_.readAsync(factorCounter.value)))

  dataIn >> inter0.dataIn // outer -> inter0
  inter0.dataOut.toFlow >> core1.dataIn // inter0 -> core1

  // core1 -> multiply twiddle factor -> inter1
  implicit val complexMultPipeline = 3
  inter1.dataIn.valid := Delay(core1.dataOut.valid, complexMultPipeline, init = False)
  val multiplied: Vec[ComplexNumber] = Vec(core1.dataOut.payload.zip(currentFactors).map { case (data, coeff) => (data * coeff).truncated(dataType) })
  inter1.dataIn.payload := multiplied

  // inter1 -> core2s
  core2s.zip(inter1.dataOut.payload.grouped(N2).toSeq).foreach { case (core, fixes) => core.dataIn.payload := Vec(fixes) }
  core2s.foreach(_.dataIn.valid := inter1.dataOut.valid)
  inter1.dataOut.ready := True
  // core2s -> inter2
  inter2.dataIn.payload := Vec(core2s.map(_.dataOut.payload.toSeq).reduce(_ ++ _))
  inter2.dataIn.valid := core2s.head.dataOut.valid
  inter2.dataOut >> dataOut // inter2 -> outer
}



