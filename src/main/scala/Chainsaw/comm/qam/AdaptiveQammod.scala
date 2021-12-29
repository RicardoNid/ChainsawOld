package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._
import scala.math.sqrt

/**
 * @param bitAlloc
 * @param powAlloc
 * @param customSymbols custom QAM values for each symOrder
 */
case class AdaptiveQammod(bitAlloc: Seq[Int], powAlloc: Seq[Double], dataType: HardType[SFix],
                          customSymbols: Map[Int, Seq[BComplex]] = Map[Int, Seq[BComplex]]())
  extends Component with DSPTestable[Bits, Vec[ComplexNumber]] {

  val complexType = toComplexType(dataType)
  override val dataIn = slave Stream Bits(bitAlloc.sum bits)
  override val dataOut = master Stream Vec(complexType, bitAlloc.size)

  logger.info(s"instantiating adaptive qammod")
  logger.info(s"bitAlloc for adaptive qammod: ${bitAlloc.mkString(" ")}")
  logger.info(s"powAlloc for adaptive qammod: ${powAlloc.mkString(" ")}")

  // extraction
  // TODO: when we have 0?
  // filtering 0s out
  val filtered = dataOut.payload.zip(bitAlloc.zip(powAlloc)).filter(_._2._1 != 0)
  val filteredOutput = filtered.map(_._1)
  val filteredBitAlloc = filtered.map(_._2._1)
  val filteredPowAlloc = filtered.map(_._2._2)

  val starts = filteredBitAlloc.indices.map(i => filteredBitAlloc.take(i).sum)
  val ends = filteredBitAlloc.indices.map(i => filteredBitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }
  printlnGreen(segments.mkString(" "))
  val segmentValues = segments.map(dataIn.payload(_).asUInt)

  // build the LUTs
  // because of powerAlloc, every segment has a different lookup table for QAM mapping
  // take custom value if provided, take Matlab value if not
  val possibleBits = 1 to bitAlloc.max
  val QAMValues = possibleBits.map(i => customSymbols.getOrElse(i, algos.Qam.getSymbols(1 << i).toSeq).toArray)

  val rmsValues = possibleBits.map(i => algos.Qam.getRms(1 << i))
  // LUTs, values determined by bitAllocated, rms and powAllocated
  val QAMLUTs = filteredBitAlloc.zipWithIndex.map { case (bitAllocated, i) =>
    val LUTValues = QAMValues(bitAllocated - 1).map(_ / rmsValues(bitAllocated - 1)).map(_ * sqrt(filteredPowAlloc(i)))
    Mem(LUTValues.map(CN(_, dataType)))
  }

  // using the input as address, reading the output from LUT(ROM)
  dataOut.payload.foreach(_ := ComplexNumber(0.0, 0.0, dataType)) // when 0 bits allocated
  dataOut.payload.allowOverride
  segmentValues.zip(QAMLUTs).zip(filteredOutput)
    .foreach { case ((addr, rom), target) => target := rom.readSync(addr) }

  dataIn.ready := True
  override val latency = 1
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}