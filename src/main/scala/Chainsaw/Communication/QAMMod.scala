package Chainsaw.Communication

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import Chainsaw.matlabIO._

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

  // because of powerAlloc, every segment has a different lookup table for QAM mapping
  val QAMValues = (1 to bitAlloc.max).map { i =>
    val M = 1 << i
    val values = (0 until M).toArray
    // take custom value if provided, take Matlab value if not
    (if (i == 1) customSymbols.getOrElse(i, Seq(new MComplex(-1, 0), new MComplex(1, 0))) // when all elements are purely real, they become Double
    else customSymbols.getOrElse(i, eng.feval[Array[MComplex]]("qammod", values, Array(M), "gray").toSeq)).toArray
  }.toArray

  val rmsValues = QAMValues.map(eng.feval[Double]("rms", _))
  // QAM LUT for each segment

  import scala.math.sqrt

  val QAMLUTs = bitAlloc.filter(_ != 0).zipWithIndex.map { case (bitAllocated, i) =>
    val LUTValues = QAMValues(bitAllocated - 1).map(_ / rmsValues(bitAllocated - 1)).map(_ * sqrt(powAlloc(i)))
    //    printlnYellow(LUTValues.mkString(" "))
    Mem(LUTValues.map(CN(_, fixedType)))
  }

  val filteredIndices = bitAlloc.zipWithIndex.filter(_._1 != 0).map(_._2)
  val starts = filteredIndices.map(i => bitAlloc.take(i).sum)
  val ends = filteredIndices.map(i => bitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }

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


