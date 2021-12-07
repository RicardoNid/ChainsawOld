package Chainsaw.comm.qam

import Chainsaw._
import Chainsaw.matlabIO._
import spinal.core._
import spinal.lib._
import scala.math.sqrt

/** Wrapper of qamdemod, supporting bitAlloc and powAlloc
 *
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

  //  val energyIn = in Vec(symbolType, bitAlloc.size) // energy value of channels from channel estimator
  val dataIn = slave Flow Vec(symbolType, bitAlloc.size)
  val dataOut = master Flow Bits(bitAlloc.sum bits)

  // segments for extracting valid outputs
  val filteredIndices = bitAlloc.zipWithIndex.filter(_._1 != 0).map(_._2)
  val starts = filteredIndices.map(i => bitAlloc.take(i).sum)
  val ends = filteredIndices.map(i => bitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }

  val demods = bitAlloc.zip(powAlloc).map{ case (bit, pow) => QAMDeModCore(symbolType, bit, sqrt(pow))}

  segments.zipWithIndex.zip(demods).foreach { case ((seg, i), core) =>
    core.dataIn.payload := dataIn.payload(i)
    dataOut.payload(seg) := core.dataOut.payload
  }
  dataOut.valid := RegNext(dataIn.valid, init = False)
}

object QAMDeMod {
  def main(args: Array[String]): Unit = {
    val bitAlloc = Seq.fill(256)(4)
    val powerAlloc = Seq.fill(256)(1.0)
    val symbolType = HardType(ComplexNumber(1, -14))
    GenRTL(QAMDeMod(bitAlloc, powerAlloc, symbolType))
    VivadoSynth(QAMDeMod(bitAlloc, powerAlloc, symbolType))
  }
}