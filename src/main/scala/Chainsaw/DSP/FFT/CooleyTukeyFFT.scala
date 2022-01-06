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
                         (implicit val cmultConfig: ComplexMultConfig)
  extends Component with DSPTestable[Vec[ComplexNumber], Vec[ComplexNumber]] {

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
  val complexMult: (ComplexNumber, Int, Int) => ComplexNumber =
    multiplyWNnk(_, _, _, dataType, coeffType)

  // define latency
  override val latency = factors.map(getDft(_).latency).sum + (factors.length - 1) * cmultConfig.pipeline

  logger.info(s"implementing a $N-point fully pipelined ${if (inverse) "ifft" else "fft"} module, latency = $latency")

  // wrapper
  dataOut.payload := Vec(Algos.cooleyTukeyBuilder(dataIn.payload, factors, block, complexMult, inverse))
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}