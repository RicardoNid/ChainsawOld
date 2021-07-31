package Chainsaw.DSP.FFT

import spinal.core._
import Chainsaw.Real

// TODO: implement
/** N-point FFT by Cooley-Tukey FFT algorithm, based on Winograd DFT algorithm, DSP with FPGA, algo 6.8, fig 6.12
 *
 * @param N length of FFT
 */
class CooleyTukeyFFT(N: Int) extends ImplicitArea[Vec[Real]] {
  override def implicitValue = ???
  override type RefOwnerType = this.type
}