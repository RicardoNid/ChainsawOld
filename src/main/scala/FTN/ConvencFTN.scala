package FTN

import spinal.core._
import spinal.lib._

// (m, n, K) convolutional encoding
// TODO: dynamic parallelFactor
// currently, n = 1 only
case class ConvencFTN(config: ConvencConfig, parallelFactor: Int) extends Component {

  import config._

  val dataIn = in Bits (parallelFactor bits)
  val dataOut = out Bits (m * parallelFactor bits)

  def octal2BinaryString(gen: Int): String =
    gen.toString.flatMap(_.asDigit.toBinaryString.reverse.padTo(3, '0').reverse)

  // TODO: avoid high fan-out
  val extractions = codeGens.map(octal2BinaryString).map(_.takeRight(constLen))
  // used to keep the last bits for next iteration
  val regs = (0 until K - 1).map(i => RegNext(dataIn(i)))

  val codeds = Vec(Bits(parallelFactor bits), m)

  (0 until m).foreach { i =>
    (0 until parallelFactor).foreach { j =>
      val bitId = parallelFactor - 1 - j
      val relatedIds = (0 until constLen).map(j - _)
      def inputOrReg(id: Int) = if (id >= 0) dataIn(parallelFactor - 1 - id) else regs(-id - 1)
      // TODO: implement the XOR by DSPs
      codeds(i)(bitId) := relatedIds.map(inputOrReg(_)) // related bits
        .zip(extractions(i)).filter(_._2 == '1').map(_._1) // filtered by the codeGen
        .xorR // XOR
    }
  }

  (0 until m * parallelFactor).foreach(i => dataOut(i) := codeds(i % m)(i / m))
}
