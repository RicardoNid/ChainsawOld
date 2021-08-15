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

/** High-throughput interlever that implements matrix interleaving
 *
 * @param row   the semantic is the same as row of Matlab matintrlv
 * @param col   the semantic is the same as row of Matlab matintrlv
 *              for matrix interleaver, the de-interleaver is an interleaver that exchange the original row and col
 * @param pFIn  data number per cycle of input, determines the throughput
 * @param pFOut data number per cycle of output, determines the throughput
 */

// TODO: implement initialization logic
// TODO: implement ping-pong logic
// TODO: implement grouped srl
// TODO: implement coloring algo
// TODO: bits should be a "datatype"
case class MatInterleaver[T <: Data](row: Int, col: Int, pFIn: Int, pFOut: Int, dataTpye: () => T) extends Component {

  val mode =
    if (pFIn == pFOut && pFIn % row == 0 && pFIn % col == 0 && (row * col) % pFIn == 0) 0
    else if (pFIn == row && pFOut == col || (pFIn == col && pFOut == row)) 1
    else 2

  val running = in Bool()
  val we = in Bool()

  val dataIn = in Bits (pFIn bits)
  val dataOut = out Bits (pFOut bits)
  //      val dataIn = in Vec(dataTpye(), pFIn)
  //      val dataOut = out Vec(dataTpye(), pFOut)

  mode match {
    case 0 => {

      val packRow = pFIn / col
      val packCol = pFIn / row
      val packSize = packRow * packCol // (intersection size of input and output)
      def packType = Bits(packSize bits)
      val squareSize = pFIn / packSize

      val rams = Seq.fill(squareSize)(Mem(packType, squareSize))
      val count = Counter(squareSize, inc = running)

      // wiring dataIn
      // TODO: examine the logic cost of this part
      val dataInRearranged = cloneOf(dataIn)
      (0 until pFIn).foreach { i =>
        val bitId = pFIn - 1 - i
        val mapped = (i % packRow) * col + i / packRow
        val mappedBitId = pFIn - 1 - mapped
        dataInRearranged(bitId) := dataIn(mappedBitId)
      }

      val dataInShifted: Bits = dataInRearranged.rotateRight(count.value << log2Up(packSize))
      val dataInPacked = dataInShifted.subdivideIn(squareSize slices).reverse // dataInPacked(0) holds the MSBs

      val dataUnpacked = Bits(pFIn bits)
      dataUnpacked.clearAll()

      // square interleave
      // TODO: extract this as a module and optimize it
      printlnGreen(s"build a $squareSize * $squareSize virtual array, using $squareSize RAMs for interleaving")
      val outputAddresses = Vec(Reg(UInt(log2Up(squareSize) bits)), squareSize)
      outputAddresses.zipWithIndex.foreach { case (addr, i) => addr.init(i) } //
      when(running) {
        when(we) {
          rams.zip(dataInPacked).foreach { case (ram, data) => ram(count.value) := data } // ram(0) holds theMSBs
        }.otherwise {
          dataUnpacked := rams.zip(outputAddresses).map { case (ram, addr) => ram.readAsync(addr) }.reverse.asBits()
          outputAddresses.zip(outputAddresses.tail :+ outputAddresses.head).foreach { case (left, right) => right := left }
        }
      }

      val dataOutShifted = dataUnpacked.rotateLeft(count.value << log2Up(packSize))

      // wiring dataOut
      (0 until pFIn).foreach { i =>
        val bitId = pFIn - 1 - i

        val packId = i / packSize
        val rowId = packId * packRow + i % packRow
        val colId = i % packSize / packRow

        val mapped = colId * row + rowId
        val mappedBitId = pFIn - 1 - mapped
        dataOut(mappedBitId) := dataOutShifted(bitId)
      }
    }
  }
}

object InterleaverFTN extends App {
  VivadoSynth(new MatInterleaver(32, 128, 256, pFOut = 256, () => UInt(8 bits)), name = "Interleaver")
  //  VivadoSynth(new InterleaverFTN(128, 32, 128), name = "DeInterleaver")
}

