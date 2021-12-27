package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.algos.Qam
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer

/** Qamdemod module for QAM2 <-> QAM256, using the constellations which are the same as Matlab
 *
 * @param symbolType    to define the QFormat of symbols in
 * @param bitsAllocated to define the modulation mode, 1 -> QAM2, 2 -> QAM4, ...
 * @param powerFactor   the symbol value is original value * power factor
 */
case class QAMDemod(symbolType: HardType[ComplexNumber],
                    bitsAllocated: Int,
                    powerFactor: Double = 1.0)
  extends Component with DSPTestable[ComplexNumber, Bits] {

  require(bitsAllocated > 0 && bitsAllocated <= 8)
  require(bitsAllocated != 5 && bitsAllocated != 7, s"QAM32/QAM128 are not supported as they can't fit the arch we use")

  val modulationOrder = 1 << bitsAllocated
  override val dataIn = slave Flow symbolType
  override val dataOut = master Flow Bits(bitsAllocated bits)

  dataIn.valid.allowPruning() // while used in an array of qamdemod modules, this may be redundant
  dataOut.valid.allowPruning()

  val maxExp = dataIn.payload.real.maxExp // in fact, this should be 1
  val minExp = dataIn.payload.real.minExp

  val stageReal = (bitsAllocated + 1) / 2 // number of stages needed to judge
  val stageImag = bitsAllocated / 2
  val rms = Qam.getRms(modulationOrder)
  // all thresholds used to compare with values, in adaptive design, they should be implemented as a RAM
  val allThresholds = (0 until stageReal - 1).map(i => (1 << (i + 1)) / rms * powerFactor)

  // generate determinants
  def genDeterminants(value: SFix, stage: Int) = {
    if (stage == 0) ArrayBuffer[Bool]()
    else {
      val dets = ArrayBuffer[SFix](value)
      val thresholds = allThresholds.take(stage - 1).reverse
      val thresholdsSF = thresholds.map(value => SF(value, maxExp exp, minExp exp))
      (0 until stage - 1).foreach(i => dets += RegNext(dets.last.abs - thresholdsSF(i)))
      // align pipeline registers
      val ret = dets.map(_.isNegative)
      ret.zipWithIndex.map { case (bool, i) => Delay(bool, stageReal - 1 - i) }
    }
  }

  val (real, imag) = (dataIn.payload.real, dataIn.payload.imag)
  val ret = (genDeterminants(real, stageReal) ++ genDeterminants(imag, stageImag)).reverse.asBits()

  override val latency = stageReal
  dataOut.payload := RegNext(~ret.msb ## ret(bitsAllocated - 2 downto 0))
  dataOut.valid := Delay(dataIn.valid, stageReal, init = False)
}

object QAMDemod {
  def main(args: Array[String]): Unit = {
    val reports = Seq(1,2,3,4,6,8).map(i => VivadoSynth(QAMDemod(HardType(ComplexNumber(1, -14)), i)))
    println(reports.map(_.LUT).mkString(" "))
    println(reports.map(_.FF).mkString(" "))
  }
}
