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
 * @param symbolType    to specify the format of output QAM symbols as fixed point complex number
 */
case class QamdemodWithAlloc(bitAlloc: Seq[Int], powAlloc: Seq[Double], symbolType: HardType[ComplexNumber])
  extends Component with DSPTestable[Vec[ComplexNumber], Bits] {

  logger.info(s"instantiating adaptive qamdemod")
  logger.info(s"bitAlloc for adaptive qamdemod: ${bitAlloc.mkString(" ")}")
  logger.info(s"powAlloc for adaptive qamdemod: ${powAlloc.mkString(" ")}")

  //  val energyIn = in Vec(symbolType, bitAlloc.size) // energy value of channels from channel estimator
  val dataIn = slave Stream Vec(symbolType, bitAlloc.size)
  val dataOut = master Stream Bits(bitAlloc.sum bits)

  // filtering 0s out
  val filtered = dataIn.payload.zip(bitAlloc.zip(powAlloc)).filter(_._2._1 != 0)
  val filteredInput = filtered.map(_._1)
  val filteredBitAlloc = filtered.map(_._2._1)
  val filteredPowAlloc = filtered.map(_._2._2)

  // instantiating qamdemod modules
  val demods = filteredBitAlloc.zip(filteredPowAlloc)
    .map { case (bit, pow) => QAMDemod(symbolType, bit, sqrt(pow)) }

  override val latency = demods.map(_.latency).max

  // allocating bits to output
  val starts = filteredBitAlloc.indices.map(i => filteredBitAlloc.take(i).sum)
  val ends = filteredBitAlloc.indices.map(i => filteredBitAlloc.take(i + 1).sum)
  val segments = ends.zip(starts).map { case (end, start) => bitAlloc.sum - 1 - start downto bitAlloc.sum - end }

  // using qamdemod modules
  segments.zipWithIndex.zip(demods).foreach { case ((seg, i), core) =>
    core.dataIn.payload := filteredInput(i)
    val segValue = Delay(core.dataOut.payload, latency - core.latency)
    segValue.setName(s"seg${i.toString.padToLeft(3, '0')}")
    dataOut.payload(seg) := segValue
  }

  dataIn.ready := True
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}