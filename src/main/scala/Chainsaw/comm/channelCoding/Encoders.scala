package Chainsaw.comm.channelCoding

import Chainsaw.DFG.FirArch.DIRECT
import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object Encoders {

  def sumOfProducts[T <: Data](add: (T, T) => T, mult: (T, T) => T, dataIns: Seq[T], coeffs: Seq[T]): T =
    dataIns.zip(coeffs).map { case (data, coeff) => mult(data, coeff) }.reduceBalancedTree(add)

  def convenc(dataIns: Seq[Bits], convConfig: ConvConfig): Seq[Bits] = {
    import convConfig._

    require(dataIns.size == n)
    dataIns.zip(ms).foreach { case (bits, m) => require(bits.getBitsWidth == m + 1, s"required $m, get ${bits.getBitsWidth}") }

    val data = dataIns.map(_.asBoolsKeepOrder).reduce(_ ++ _)
    val coeffs = (0 until k).map(i => binaryCodeGens.map(_ (i)) // all generators for output i
      .map(_.map(char => if (char == '1') True else False)).reduce(_ ++ _)) // string to bools
    require(data.size == coeffs.head.size, s"data size ${data.size}, coeff size ${coeffs.head.size}")

    coeffs.map(coeff => sumOfProducts[Bool](_ ^ _, _ & _, data, coeff).asBits)
  }

  def convencDFG(dataIn: Seq[Bits], convConfig: ConvConfig): Seq[Bits] = {
    import convConfig._

    val and: BinaryNode[Bits] = BinaryNode(Operators.and, "and")
    val xor: BinaryNode[Bits] = BinaryNode(Operators.xor, "xor")
    def convDirect(coeffs: Seq[Int]): DFGGraph[Bits] = FIRGen(xor, and, DIRECT, coeffs, 1 bits, 1).getGraph

    val convMatrix = Seq.tabulate(n,k) {(i, j) =>
      val gen = binaryCodeGens(i)(j).reverse.map(_.asDigit)
      convDirect(gen).impl(Seq(dataIn(i))).head
    }

    (0 until k).map(i => (0 until n).map(j => convMatrix(j)(i)).reduce(_ ^ _))
  }
}

case class ConvEncoder(convConfig: ConvConfig) extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {

  import convConfig._

  // testable interface
  val clear: Bool = in Bool()
  override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(1 bits), n)
  override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(1 bits), k)
  override val latency: Int = 0

  // registers and shifting logic
  val regs: Array[Bits] = ms.map(m => RegInit(B(0, m bits)))
  //  when(clear)(regs.foreach(_.clearAll()))
  //    .otherwise(regs.zip(dataIn.payload).foreach { case (bits, bool) => bits := bool ## (bits >> 1) })
  regs.zip(dataIn.payload).foreach { case (bits, bool) => bits := bool ## (bits >> 1) }
  val data: IndexedSeq[Bits] = regs.zip(dataIn.payload).map { case (bits, bool) => bool ## bits }
  data.zipWithIndex.foreach { case (bits, i) => bits.setName(s"data$i") }
  dataOut.payload := Vec(Encoders.convenc(data, convConfig))
  dataOut.valid := dataIn.valid
}

case class ConvEncoderDFG(convConfig: ConvConfig) extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
  import convConfig._
  override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(1 bits), n)
  override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(), k)
  override val latency: Int = 0
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
  dataOut.payload := Vec(Encoders.convencDFG(dataIn.payload, convConfig))
  override type RefOwnerType = this.type
}