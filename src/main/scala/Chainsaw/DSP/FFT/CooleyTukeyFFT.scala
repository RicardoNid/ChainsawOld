package Chainsaw.DSP.FFT

import Chainsaw._
import Chainsaw.algos.Permutations.{matIntrlv, transpose}
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

/** implement fft hardware by CooleyTukey indexing method, the output is digit-reversed
 *
 * @param N           length of fft/ifft
 * @param inverse     fft/ifft
 * @param dataType    hard type for datapath
 * @param coeffType   hard type for coeffs
 * @param factors     factors for decomposition, for each factor, the corresponding DFT algo must exist
 * @param shifts      shifts(carries) on each stage, by default all 0s(then you should provide a wide enough dataType to avoid all overflows)
 * @param cmultConfig configuration of complex multiplication
 * @see ''Fast Algorithms for Signal Processing'' Chap3.1
 */
@coreHardware
@fullyPipelined
case class CooleyTukeyFFT(N: Int, inverse: Boolean, // determining the transformation
                          dataType: HardType[SFix], coeffType: HardType[SFix], // determining the precision
                          factors: Seq[Int], shifts: Seq[Int] = null) // determining the decomposition and precision on each stave
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

  logger.info(s"implementing a $N-point fully paralleled ${if (inverse) "ifft" else "fft"} module as ${factors.mkString(" * ")}")

  // checking for validity
  require(factors.product == N)
  if (shifts != null) require(shifts.length == factors.length)
  if (coeffType().maxExp != 1) logger.warn(s"range of fft/ifft coeffs is [-1, 1], ${coeffType().maxExp} bits for integral part is redundant")

  // determining shifts
  val shiftOnStages = if (shifts == null) Seq.fill(factors.length)(0) else shifts

  // I/O
  val complexDataType = toComplexType(dataType)
  override val dataIn = slave Flow Vec(complexDataType(), N)

  val retDataType = getShiftedType(dataType, shiftOnStages.sum)
  override val dataOut = master Flow Vec(toComplexType(retDataType), N)

  // building blocks: block(DFT) and twiddle(multiplied by coeffs)
  def doBlock(input: Seq[ComplexNumber], shift: Int): Seq[ComplexNumber] = {
    val expandedType = HardType(SFix((input.head.real.maxExp + shift) exp, input.head.real.minExp exp))
    val dft = DFT(input.size, inverse, expandedType, coeffType)
    dft.dataIn := Vec(input.map(getExpanded(_, shift)))
    dft.dataOut.map(getReduced(_, shift))
  }

  def doTwiddle(input: Seq[Seq[ComplexNumber]]) = {
    def indices(N1: Int, N2: Int) = Seq.tabulate(N2, N1)((n2, k1) => n2 * k1)

    val dataType = input.head.head.realType
    val N1 = input.head.size
    val N2 = input.size
    input.zip(indices(N1, N2))
      .map { case (ts, ints) => ts.zip(ints)
        .map { case (t, i) => multiplyWNnk(t, if (!inverse) i else -i, N1 * N2, dataType, coeffType) }
      }
  }

  // recursively build the module
  def build(input: Seq[ComplexNumber], factors: Seq[Int], shifts: Seq[Int]): Seq[ComplexNumber] = {

    if (factors.size == 1) doBlock(input, shifts.head)
    else {
      val N1 = factors.head
      val N2 = input.size / N1
      // TODO: better strategy(current strategy makes some bits totally useless)
      // operations on a stage
      val input2D = matIntrlv(input, N1, N2).grouped(N1).toSeq // permutation 0
      val afterBlock = input2D.map(doBlock(_, shifts.head))
      val afterParallel = doTwiddle(afterBlock)
      val input2DForRecursion = transpose(afterParallel) // permutation 1(transpose)
      // recursively call other stages
      val afterRecursion = input2DForRecursion.map(build(_, factors.tail, shifts.tail))
      val ret = matIntrlv(afterRecursion.flatten, N1, N2) // permutation 2
      ret
    }
  }

  // latency = latency of DFTs + latency of twiddles
  val getDft: Int => DFT = DFT(_, inverse, dataType, coeffType)
  override val latency = factors.map(getDft(_).latency).sum + (factors.length - 1) * cmultConfig.pipeline

  // wrapper
  dataOut.payload := Vec(build(dataIn.payload, factors, shifts))
  dataOut.valid := Delay(dataIn.valid, latency, init = False)

  // utils for shifts
  def getShiftedType(dataType: HardType[SFix], shift: Int) = HardType(dataType() << shift)

  def getExpanded(data: ComplexNumber, shift: Int) = {
    val newType = HardType(SFix((data.real.maxExp + shift) exp, data.real.minExp exp))
    data.truncated(newType)
  }

  def getReduced(data: ComplexNumber, shift: Int) = {
    val newType = HardType(SFix(data.real.maxExp exp, (data.real.minExp + shift) exp))
    data.truncated(newType)
  }
}