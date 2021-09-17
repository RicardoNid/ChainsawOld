package Chainsaw.Communication.qam

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._
import scala.math.sqrt

/**
 * @param bitAlloc      the list of symOrder for each segment
 * @param symbolType    to specify the format of output QAM symbols as fixed point complex number
 * @param gray          symbol order, binary when false, gray when true
 * @param customSymbols when you need to specify custom QAM values(different from Matlab)
 */
case class QAMMod(bitAlloc: Seq[Int], powAlloc: Seq[Double], symbolType: HardType[ComplexNumber],
                  gray: Boolean = true, customSymbols: Map[Int, Seq[MComplex]] = Map[Int, Seq[MComplex]]()) extends Component {

  val fixedType = HardType(symbolType().real)
  val dataIn = slave Flow Bits(bitAlloc.sum bits)
  val dataOut = master Flow Vec(symbolType, bitAlloc.size)

  // segments for extracting valid inputs
  val filteredIndices = bitAlloc.zipWithIndex.filter(_._1 != 0).map(_._2)
  val starts = filteredIndices.map(i => bitAlloc.take(i).sum)
  val ends = filteredIndices.map(i => bitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }

  // build the LUTs
  // because of powerAlloc, every segment has a different lookup table for QAM mapping
  // take custom value if provided, take Matlab value if not
  val possibleBits = 1 to bitAlloc.max
  val QAMValues = possibleBits.map(i => customSymbols.getOrElse(i, Refs.getQAMValues(i).toSeq).toArray)
  val rmsValues = possibleBits.map(Refs.getQAMRms)
  // LUTs, values determined by bitAllocated, rms and powAllocated
  val QAMLUTs = bitAlloc.filter(_ != 0).zipWithIndex.map { case (bitAllocated, i) =>
    val LUTValues = QAMValues(bitAllocated - 1).map(_ / rmsValues(bitAllocated - 1)).map(_ * sqrt(powAlloc(i)))
    Mem(LUTValues.map(CN(_, fixedType)))
  }

  // using the input as address, reading the output from LUT(ROM)
  dataOut.payload.foreach(_ := ComplexNumber(0.0, 0.0, fixedType))
  dataOut.payload.foreach(_.allowOverride)
  bitAlloc.filter(_ != 0).indices.foreach { i =>
    dataOut.payload(i) := QAMLUTs(i).readSync(dataIn.payload(segments(i)).asUInt)
  }

  dataOut.valid := RegNext(dataIn.valid, init = False)
}

object QAMMod {

  def main(args: Array[String]): Unit = {
    val bitAlloc = Seq.fill(256)(4)
    val powerAlloc = Seq.fill(256)(1.0)
    val symbolType = HardType(ComplexNumber(1, -14))
    GenRTL(QAMMod(bitAlloc, powerAlloc, symbolType))
    VivadoSynth(QAMMod(bitAlloc, powerAlloc, symbolType))
  }
}


