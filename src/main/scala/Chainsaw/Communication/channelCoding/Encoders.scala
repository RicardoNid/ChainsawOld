package Chainsaw.Communication.channelCoding

import spinal.core._
import spinal.lib._
import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

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
