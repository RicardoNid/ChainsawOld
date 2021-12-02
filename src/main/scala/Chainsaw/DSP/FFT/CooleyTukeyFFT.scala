package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.matlabIO.MComplex
import spinal.core._
import spinal.lib._

case class CooleyTukeyFFT(N: Int = 256, factors: Seq[Int], inverse: Boolean = false,
                          dataType: HardType[SFix], coeffType: HardType[SFix]) extends Component {

  val complexDataType = HardType(ComplexNumber(dataType().maxExp, dataType().minExp))
  val dataIn = in Vec(complexDataType, N) // complex number stored in bits
  val dataOut = out Vec(complexDataType, N) // complex number stored in bits

  def block(input: Seq[ComplexNumber]): Seq[ComplexNumber] = { // TODO: Implement DFTN, N is power of prime number
    val dft = DFT(input.size, inverse, dataType, coeffType)
    dft.dataIn := Vec(input)
    dft.dataOut
  }

  val mult: (ComplexNumber, Int, Int) => ComplexNumber = multiplyWNnk(_, _, _, dataType, coeffType)(latency = 3)
  dataOut := Vec(Algos.cooleyTukeyBuilder(dataIn, factors, block, mult, inverse))

  def latency = LatencyAnalysis(dataIn(0).real.raw, dataOut(0).real.raw)
}

case class CooleyTukeyFFTStream(N: Int = 256, factors: Seq[Int], inverse: Boolean = false,
                                dataType: HardType[SFix], coeffType: HardType[SFix]) extends Component {
  val core = CooleyTukeyFFT(N, factors, inverse, dataType, coeffType)
  val dataIn = slave(Stream(core.dataIn))
  val dataOut = master(Stream(core.dataOut))

  core.dataIn := dataIn.payload
  dataOut.payload := core.dataOut

  dataIn.ready := True
  dataOut.valid := Delay(dataIn.valid, core.latency, init = False)

  def latency = core.latency
}

case class CooleyTukeyBackToBack(
                                  N: Int = 256, pF: Int = 32,
                                  factors1: Seq[Int] = Seq(4, 4, 2), factors2: Seq[Int] = Seq(4, 2),
                                  inverse: Boolean = false,
                                  dataType: HardType[SFix], coeffType: HardType[SFix])
  extends Component {

  val complexDataType = HardType(ComplexNumber(dataType))
  val N1 = pF
  val N2 = N / pF

  val core1 = CooleyTukeyFFTStream(N1, factors1, inverse, dataType, coeffType)
  val core2s = Seq.fill(N1 / N2)(CooleyTukeyFFTStream(N2, factors2, inverse, dataType, coeffType))
  val inter0 = DSP.interleave.MatIntrlv(N1, N2, pF, pF, complexDataType)
  val inter1 = DSP.interleave.MatIntrlv(N2, N1, pF, pF, complexDataType)
  val inter2 = DSP.interleave.MatIntrlv(N1, N2, pF, pF, complexDataType)

  val dataIn = slave Stream Vec(complexDataType, pF)
  val dataOut = master Stream Vec(complexDataType, pF)

  //  val twiddleFactors: Seq[Seq[MComplex]] = Algos.cooleyTukeyCoeffIndices(N1, N2).map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
  val twiddleFactors: Seq[Seq[MComplex]] = Algos.cooleyTukeyCoeffIndices(N2, N1).map(_.map(i => if (inverse) WNnk(N, -i) else WNnk(N, i)))
  //  val twiddleFactorROM: Mem[Vec[ComplexNumber]] = Mem(twiddleFactors.map(vec => Vec(vec.map(CN(_, coeffType)))))
  val twiddleFactorROMs = twiddleFactors.map(vec => Mem(Vec(vec.map(CN(_, coeffType)))))
  val factorCounter = Counter(N2, inc = core1.dataOut.fire)
  logger.info(s"twiddle factors num: $N1 * $N2")
  //  val currentFactors: Vec[ComplexNumber] = twiddleFactorROM.readAsync(factorCounter.value)
  val currentFactors: Vec[ComplexNumber] = Vec(twiddleFactorROMs.map(_.readAsync(factorCounter.value)))

  dataIn >> inter0.dataIn // outer -> inter0
  inter0.dataOut >> core1.dataIn // inter0 -> core1

  // core1 -> multiply twiddle factor -> inter1
  implicit val complexMultPipeline = 3
  inter1.dataIn.valid := Delay(core1.dataOut.valid, complexMultPipeline, init = False)
  val multiplied: Vec[ComplexNumber] = Vec(core1.dataOut.payload.zip(currentFactors).map { case (data, coeff) => (data * coeff).truncated(dataType) })
  inter1.dataIn.payload := multiplied
  core1.dataOut.ready := True

  // inter1 -> core2s
  core2s.zip(inter1.dataOut.payload.grouped(N2).toSeq).foreach { case (core, fixes) => core.dataIn.payload := Vec(fixes) }
  core2s.foreach(_.dataIn.valid := inter1.dataOut.valid)
  inter1.dataOut.ready := core2s.head.dataIn.ready
  // core2s -> inter2
  inter2.dataIn.payload := Vec(core2s.map(_.dataOut.payload.toSeq).reduce(_ ++ _))
  inter2.dataIn.valid := core2s.head.dataOut.valid
  core2s.foreach(_.dataOut.ready := inter2.dataIn.ready)
  inter2.dataOut >> dataOut // inter2 -> outer

  println(LatencyAnalysis(dataIn.payload(0).real.raw, dataOut.payload(0).real.raw))

  def latency = inter0.latency + core1.latency + inter1.latency + core2s.head.latency + +inter2.latency

  printlnYellow(s"CooleyTukey BackToBack $N1 * $N2, latency = $latency")
}

object CooleyTukeyFFT extends App {
  // length = 256, throughput = 1/8
  val dataType = HardType(SFix(4 exp, -11 exp))
  val coeffType = HardType(SFix(1 exp, -11 exp))
  VivadoSynth(new CooleyTukeyBackToBack(512, 256, Seq(4, 4, 4, 2), Seq(2), inverse = true, dataType, coeffType))
  //    VivadoSynth(CooleyTukeyFFT(512, Seq.fill(7)(2), false, SFix(1 exp, -6 exp), SFix(1 exp, -6 exp)))
  //  VivadoSynth(new CooleyTukeyFFT(N = 256, dataWidth = 16, coeffWidth = 16, factors = Seq.fill(4)(4), inverse = true), name = "radix4IFFT256")
}
