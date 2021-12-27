package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import scala.math.sqrt

/**
 * @param bitAlloc
 * @param powAlloc
 * @param customSymbols custom QAM values for each symOrder
 */
case class AdaptiveQammod(bitAlloc: Seq[Int], powAlloc: Seq[Double],
                          dataType: HardType[SFix],
                          customSymbols: Map[Int, Seq[BComplex]] = Map[Int, Seq[BComplex]]())
  extends Component with DSPTestable[Bits, Vec[ComplexNumber]] {

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Bits(bitAlloc.sum bits)
  override val dataOut = master Stream Vec(complexType, bitAlloc.size)

  // extraction
  // TODO: when we have 0?
  val filteredIndices = bitAlloc.zipWithIndex.map(_._2)
  val starts = filteredIndices.map(i => bitAlloc.take(i).sum)
  val ends = filteredIndices.map(i => bitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }
  println(segments.mkString(" "))
  val segmentValues = segments.map(dataIn.payload(_).asUInt)

  // build the LUTs
  // because of powerAlloc, every segment has a different lookup table for QAM mapping
  // take custom value if provided, take Matlab value if not
  val possibleBits = 1 to bitAlloc.max
  val QAMValues = possibleBits.map(i => customSymbols.getOrElse(i, algos.Qam.getSymbols(1 << i).toSeq).toArray)

  val rmsValues = possibleBits.map(i => algos.Qam.getRms(1 << i))
  // LUTs, values determined by bitAllocated, rms and powAllocated
  val QAMLUTs = bitAlloc.zipWithIndex.map { case (bitAllocated, i) =>
    if (bitAllocated == 0) null
    else {
      val LUTValues = QAMValues(bitAllocated - 1).map(_ / rmsValues(bitAllocated - 1)).map(_ * sqrt(powAlloc(i)))
      Mem(LUTValues.map(CN(_, dataType)))
    }
  }

  // using the input as address, reading the output from LUT(ROM)
  dataOut.payload.foreach(_ := ComplexNumber(0.0, 0.0, dataType)) // when 0 bits allocated
  dataOut.payload.allowOverride
  segmentValues.zip(QAMLUTs).zip(dataOut.payload).foreach{ case ((addr, rom), target) =>
    if (rom == null) Unit // do nothing
    else target := rom.readSync(addr)
  }

  dataIn.ready := True
  override val latency = 1
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}

object AdaptiveQammod {

  def main(args: Array[String]): Unit = {
    val bitAlloc = Seq.fill(256)(4)
    val powerAlloc = Seq.fill(256)(1.0)
    val dataType = HardType(SFix(1 exp, -14 exp))
    GenRTL(AdaptiveQammod(bitAlloc, powerAlloc, dataType))
    VivadoSynth(AdaptiveQammod(bitAlloc, powerAlloc, dataType))
  }
}


