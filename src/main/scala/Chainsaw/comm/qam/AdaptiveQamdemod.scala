package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

import scala.math.sqrt
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

/** Wrapper of qamdemod, supporting bitAlloc and powAlloc
 *
 * @param bitAlloc      the list of symOrder for each segment
 * @param powAlloc      the list of power allocation from each symbol
 * @param energyType    to specify the format of input energy value
 * @param symbolType    to specify the format of output QAM symbols as fixed point complex number
 * @param gray          symbol order, binary when false, gray when true
 * @param customSymbols when you need to specify custom QAM values(different from Matlab)
 */
case class AdaptiveQamdemod(bitAlloc: Seq[Int], powAlloc: Seq[Double], symbolType: HardType[ComplexNumber],
                            gray: Boolean = true, customSymbols: Map[Int, Seq[BComplex]] = Map[Int, Seq[BComplex]]())
  extends Component with DSPTestable[Vec[ComplexNumber], Bits] {

  val fixedType = HardType(symbolType().real)

  //  val energyIn = in Vec(symbolType, bitAlloc.size) // energy value of channels from channel estimator
  val dataIn = slave Stream Vec(symbolType, bitAlloc.size)
  val dataOut = master Stream Bits(bitAlloc.sum bits)

  // segments for extracting valid outputs
  val filteredIndices = bitAlloc.zipWithIndex.filter(_._1 != 0).map(_._2)
  val starts = filteredIndices.map(i => bitAlloc.take(i).sum)
  val ends = filteredIndices.map(i => bitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }

  val demods = bitAlloc.zip(powAlloc).map { case (bit, pow) => QAMDemod(symbolType, bit, sqrt(pow)) }
  override val latency = demods.map(_.latency).max

  logger.info(s"bitAlloc for adaptive qamdemod: ${bitAlloc.mkString(" ")}")
  logger.info(s"powAlloc for adaptive qamdemod: ${powAlloc.mkString(" ")}")

  segments.zipWithIndex.zip(demods).foreach { case ((seg, i), core) =>
    core.dataIn.payload := dataIn.payload(i)
    val segValue = Delay(core.dataOut.payload, latency - core.latency)
    segValue.setName(s"seg${i.toString.padToLeft(3, '0')}")
    dataOut.payload(seg) := segValue
  }

  dataIn.ready := True
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}

object AdaptiveQamdemod {
  def main(args: Array[String]): Unit = {
    val bitAlloc = Seq.fill(256)(4)
    val powerAlloc = Seq.fill(256)(1.0)
    val symbolType = HardType(ComplexNumber(1, -14))
    GenRTL(AdaptiveQamdemod(bitAlloc, powerAlloc, symbolType))
    VivadoSynth(AdaptiveQamdemod(bitAlloc, powerAlloc, symbolType))
  }
}