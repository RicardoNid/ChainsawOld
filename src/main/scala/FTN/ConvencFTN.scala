package FTN

import spinal.core._
import spinal.lib._
import Chainsaw._

// (m, n, K) convolutional encoding
// TODO: dynamic parallelFactor
// currently, n = 1 only
case class ConvencFTN(config: ConvencConfig, pF: Int) extends Component {

  import config._

  val dataIn = slave Flow Fragment(Bits(pF bits))
  val dataOut = master Flow Fragment(Bits(m * pF bits))

  /** Convert octal number(represented by decimal Int) to binary string
   * @example 133 -> 1011011
   */
  def octal2BinaryString(octal: Int): String = {
    val octalValue = octal.toString.map(_.asDigit).reverse.zipWithIndex.map{ case (digit, exp) => digit * (1 << (3 * exp))}.sum
    octal.toString.flatMap(_.asDigit.toBinaryString.padToLeft(3, '0')).takeRight(log2Up(octalValue))
  }
  def convEncode(bits: Bits, codeGen: String) = bits.asBools.zip(codeGen).filter(_._2 == '1').map(_._1).xorR // convenc

  // used to keep the last bits for next cycle
  val regs = RegInit(B(0, K - 1 bits))
  when(dataIn.last)(regs := B(0, K - 1 bits))
    .elsewhen(dataIn.fire)(regs := dataIn.fragment(K - 2 downto 0))

  val concatenated = regs ## dataIn.fragment

  Seq.tabulate(pF, m) { (j, i) =>
    val codeGen = octal2BinaryString(codeGens(i))
    dataOut.fragment((j + 1) * m - 1 - i) := RegNext(convEncode(concatenated(j + K - 1 downto j), codeGen))
  }

  dataOut.last := RegNext(dataIn.last, init = False)
  dataOut.valid := RegNext(dataIn.valid, init = False)

  def latency = 1
}
