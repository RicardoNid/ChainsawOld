package Chainsaw.DSP.interleave

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.Real
import Chainsaw._
import Chainsaw.dspTest.DSPTestable
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/** High-throughput general interlever that implements matrix interleaving with any given parameters
 *
 * @param row   the semantic is the same as row of Matlab matintrlv
 * @param col   the semantic is the same as row of Matlab matintrlv
 *              for matrix interleaver, the de-interleaver is an interleaver that exchange the original row and col
 * @param pFIn  data number per cycle of input, determines the throughput
 * @param pFOut data number per cycle of output, determines the throughput
 * @see [[https://ieeexplore.ieee.org/document/6732285]], though my implementation is similar,I proposed it all by myself
 */

case class AdaptiveMatIntrlv[T <: Data](
                                         row: Int, col: Int,
                                         pFIn: Int, pFOut: Int,
                                         dataType: HardType[T])
  extends Component with DSPTestable[Vec[T], Vec[T]] {

  val mode =
    if (pFIn == col && pFOut == row) 0
    else if (pFIn == pFOut && pFIn % row == 0 && pFIn % col == 0 && (row * col) % pFIn == 0) 1
    else if (pFIn == row && pFOut == col || (pFIn == col && pFOut == row)) 2
    else 3

  logger.info(s"implementing a $row * $col adaptive matIntrlv at parallelism ($pFIn, $pFOut) of ${dataType.getBitsWidth}-bits-width elements by mode $mode")

  override val dataIn = slave Stream Vec(dataType, pFIn)
  override val dataOut = master Stream Vec(dataType, pFOut)

  val core = mode match {
    case 0 => // directly using the core
      val core = MatIntrlv(row, col, dataType)
      core.dataIn << dataIn
      core.dataOut >> dataOut
      core
    case 1 => // packing + core
      // parameters for packing
      val packRow = pFIn / col
      val packCol = pFIn / row
      val packSize = packRow * packCol // (intersection size of input and output)
      val squareSize = pFIn / packSize

      val packType = HardType(Bits(packSize * widthOf(dataType) bits))

      val core = MatIntrlv(squareSize, squareSize, packType)

      // packing input
      val dataInRearranged: Seq[T] = Algos.matIntrlv(dataIn.payload, packRow, col)
      val dataInPacked = Vec(dataInRearranged.grouped(packSize).toSeq.map(_.asBits()))

      // connecting the core
      core.dataIn.valid := dataIn.valid
      dataIn.ready := core.dataIn.ready
      core.dataIn.payload := dataInPacked

      def unpack(in : Vec[Bits]) = {
        val ret = cloneOf(dataOut.payload)
        (0 until row / packRow).foreach { packId =>
          (0 until packCol).foreach { packColId =>
            (0 until packRow).foreach { packRowId =>
              val id = packColId * row + packId * packRow + packRowId
              val idInPack = packColId * packRow + packRowId
              ret(id).assignFromBits(in(packId).subdivideIn(packSize slices)(idInPack))
            }
          }
        }
        ret
      }

      core.dataOut.t(unpack) >> dataOut
      core
    case _ => throw new IllegalArgumentException(s"mode $mode has not been implemented yet")
  }

  override val latency = core.latency

  logger.info(s"adaptive matIntrlv latency = $latency")
}