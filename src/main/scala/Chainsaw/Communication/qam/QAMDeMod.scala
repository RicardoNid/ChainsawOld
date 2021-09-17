package Chainsaw.Communication.qam

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._

/**
 * @param bitAlloc      the list of symOrder for each segment
 * @param powAlloc      the list of power allocation from each symbol
 * @param energyType    to specify the format of input energy value
 * @param symbolType    to specify the format of output QAM symbols as fixed point complex number
 * @param gray          symbol order, binary when false, gray when true
 * @param customSymbols when you need to specify custom QAM values(different from Matlab)
 */
case class QAMDeMod(bitAlloc: Seq[Int], powAlloc: Seq[Double], symbolType: HardType[ComplexNumber],
                    gray: Boolean = true, customSymbols: Map[Int, Seq[MComplex]] = Map[Int, Seq[MComplex]]()) extends Component {

  val fixedType = HardType(symbolType().real)

  val energyIn = in Vec(symbolType, bitAlloc.size) // energy value of channels from channel estimator
  val dataIn = slave Flow Vec(symbolType, bitAlloc.size)
  val dataOut = master Flow Bits(bitAlloc.sum bits)

  // segments for extracting valid outputs
  val filteredIndices = bitAlloc.zipWithIndex.filter(_._1 != 0).map(_._2)
  val starts = filteredIndices.map(i => bitAlloc.take(i).sum)
  val ends = filteredIndices.map(i => bitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }

  val possibleBits = 1 to bitAlloc.max
  val QAMValues = possibleBits.map(i => customSymbols.getOrElse(i, Refs.getQAMValues(i).toSeq).toArray)
  val rmsValues = possibleBits.map(Refs.getQAMRms)

  dataOut.valid := RegNext(dataIn.valid, init = False)
}

object QAMDeMod {

  def main(args: Array[String]): Unit = {

  }
}



