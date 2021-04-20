package DSP

import DSP.WinogradDFT.winogradDFT
import spinal.core._
import spinal.lib.{Delay, master, slave}

/** N-point DFT by Winograd DFT algorithm, On Computing the Discrete Fourier Transform, P18
 *
 * @param N - length of DFT
 */
class WinogradDFT(N: Int) extends Component with DSPDesignOld {

  require(isPrime(N), s"Winograd DFT is for prime number")
  require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

  val io = new Bundle {
    val input = slave Flow (Vec(globalType, N * 2))
    val output = master Flow (Vec(globalType, N * 2))
  }

  val inputNumbers = (0 until N).map(i => ComplexNumber(io.input.payload(i * 2), io.input.payload(i * 2 + 1)))

  val outputNumbers = winogradDFT(inputNumbers)
  (0 until N).foreach { i =>
    io.output.payload(2 * i) := outputNumbers(i).real.truncated
    io.output.payload(2 * i + 1) := outputNumbers(i).imag.truncated
  }

  io.output.valid := Delay(io.input.valid, 1)
  io.output.valid.init(False)

  override def getDelay: Int = 0
}

object WinogradDFT {

  // TODO: Wino should be combined with hardware architecture
  def winogradDFT(input: IndexedSeq[ComplexNumber]) = {

    val N = input.length

    require(isPrime(N), s"Winograd DFT is for prime number")
    require(Set(2).contains(N), s"$N point Winograd DFT will be supported in later release")

    val output = Array.ofDim[ComplexNumber](N)

    N match {
      case 2 =>{
        output(0) = input(0) + input(1)
        output(1) = input(0) - input(1)
      }
    }

    output.map(_.tap)
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig(mode = SystemVerilog)
      .generate(new WinogradDFT(4))
  }
}


