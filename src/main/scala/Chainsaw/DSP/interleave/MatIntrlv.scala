package Chainsaw.DSP.interleave

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

import Chainsaw._
import spinal.core._
import spinal.lib._

/** High-throughput general interlever that implements matrix interleaving with any given parameters
 *
 * @param row   the semantic is the same as row of Matlab matintrlv
 * @param col   the semantic is the same as row of Matlab matintrlv
 *              for matrix interleaver, the de-interleaver is an interleaver that exchange the original row and col
 * @param pFIn  data number per cycle of input, determines the throughput
 * @param pFOut data number per cycle of output, determines the throughput
 */

// TODO: implement reset/initialization logic
// TODO: implement coloring algo
case class MatIntrlv[T <: Data](row: Int, col: Int, pFIn: Int, pFOut: Int, dataType: HardType[T]) extends Component {

  val mode =
    if (pFIn == col && pFOut == row) 0
    else if (pFIn == pFOut && pFIn % row == 0 && pFIn % col == 0 && (row * col) % pFIn == 0) 1
    else if (pFIn == row && pFOut == col || (pFIn == col && pFOut == row)) 2
    else 3

  printlnGreen(s"interleaver is on mode $mode")

  val dataIn = slave Stream Vec(dataType, pFIn)
  val dataOut = master Stream Vec(dataType, pFOut)

  var latency = 0

  mode match {
    case 0 => // directly using the core
      val core = MatIntrlvCore(row, col, dataType)
      core.dataIn << dataIn
      core.dataOut >> dataOut
      latency = row max col

    case 1 => // packing + core
      // parameters for packing
      val packRow = pFIn / col
      val packCol = pFIn / row
      val packSize = packRow * packCol // (intersection size of input and output)
      val squareSize = pFIn / packSize

      val packType = HardType(Bits(packSize * widthOf(dataType) bits))

      val core = MatIntrlvCore(squareSize, squareSize, packType)
      latency = squareSize

      // packing input
      val dataInRearranged: Seq[T] = Algos.matIntrlv(dataIn.payload, packRow, col)
      val dataInPacked = Vec(dataInRearranged.grouped(packSize).toSeq.map(_.asBits()))
      val dataOutPacked = cloneOf(dataInPacked)

      // connecting the core
      core.dataIn.valid := dataIn.valid
      dataIn.ready := core.dataIn.ready

      core.dataIn.payload := dataInPacked
      dataOutPacked := core.dataOut.payload

      dataOut.valid := core.dataOut.valid
      core.dataOut.ready := dataOut.ready

      // unpacking output
      (0 until row / packRow).foreach { packId =>
        (0 until packCol).foreach { packColId =>
          (0 until packRow).foreach { packRowId =>
            val id = packColId * row + packId * packRow + packRowId
            val idInPack = packColId * packRow + packRowId
            dataOut.payload(id).assignFromBits(dataOutPacked(packId).subdivideIn(packSize slices)(idInPack))
          }
        }
      }
  }
}

object InterleaverFTN extends App {
  VivadoSynth(new MatIntrlv(32, 128, 256, pFOut = 256, HardType(Bits(1 bits))), name = "Interleaver")
  //  VivadoSynth(new InterleaverFTN(128, 32, 128), name = "DeInterleaver")
}

