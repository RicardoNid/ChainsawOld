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
case class CooleyTukeyFFT(N: Int, factors: Seq[Int], inverse: Boolean,
                          dataType: HardType[SFix], coeffType: HardType[SFix],
                          shifts: Seq[Int] = null)
                         (implicit val cmultConfig: ComplexMultConfig)
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  val complexDataType = toComplexType(dataType)

  val shiftOnStages = if (shifts == null) Seq.fill(factors.length)(0) else shifts
  def getShiftedType(dataType: HardType[SFix], shift: Int) = HardType(dataType() << shift)

  val retDataType = getShiftedType(dataType, shiftOnStages.sum)

  override val dataIn = slave Flow Vec(complexDataType(), N)
  override val dataOut = master Flow Vec(toComplexType(retDataType), N)

  val getDft: Int => DFT = DFT(_, inverse, dataType, coeffType)

  def indices(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)

  // using DFT as building blocks
  def doBlock(input: Seq[ComplexNumber]): Seq[ComplexNumber] = {
    val dataType = input.head.realType
    val dft = DFT(input.size, inverse, dataType, coeffType)
    dft.dataIn := Vec(input)
    dft.dataOut
  }

  def doTwiddle(input: Seq[Seq[ComplexNumber]]) = {
    val dataType = input.head.head.realType
    val N1 = input.head.size
    val N2 = input.size
    input.zip(indices(N1, N2))
      .map { case (ts, ints) => ts.zip(ints)
        .map { case (t, i) => multiplyWNnk(t, if (!inverse) i else -i, N1 * N2, dataType, coeffType) }
      }
  }

  def getShiftedVec(input: Seq[ComplexNumber], shift: Int) =
    input.map(_.truncated(getShiftedType(input.head.realType, shift)))

  def build(input: Seq[ComplexNumber], factors: Seq[Int]): Seq[ComplexNumber] = {

    if (factors.size == 1) getShiftedVec(doBlock(input), shiftOnStages.last)
    else {
      val N1 = factors.head
      val N2 = input.size / N1
      import DSP.interleave.Algos._
      // TODO: better strategy(current strategy makes some bits totally useless)
      val input2D = matIntrlv(input, N1, N2).grouped(N1).toSeq // permutation 0
        .map(getShiftedVec(_, shiftOnStages(shiftOnStages.length - factors.length)))
      val afterBlock = input2D.map(doBlock)

      val afterParallel = doTwiddle(afterBlock)
      val input2DForRecursion = transpose(afterParallel) // permutation 1(transpose)
      val afterRecursion = input2DForRecursion.map(build(_, factors.tail))
      val ret = matIntrlv(afterRecursion.flatten, N1, N2) // permutation 2
      ret
    }
  }

  // define latency
  override val latency = factors.map(getDft(_).latency).sum + (factors.length - 1) * cmultConfig.pipeline

  logger.info(s"implementing a $N-point fully pipelined ${if (inverse) "ifft" else "fft"} module, latency = $latency")

  // wrapper
  dataOut.payload := Vec(build(dataIn.payload, factors))
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}