package Chainsaw.DSP

import spinal.core._
import Chainsaw.Real

// TODO: implement
/** N-point DFT by Winograd DFT algorithm, On Computing the Discrete Fourier Transform, P18
 *
 * @param N - length of DFT
 */
class WinogradDFT(N: Int) extends ImplicitArea[Vec[Real]] {
  override def implicitValue = ???
  override type RefOwnerType = this.type
}


