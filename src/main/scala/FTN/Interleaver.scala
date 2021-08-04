package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

/** High-throughput interlever that implements matrix interleaving
 * @param row
 * @param col
 * for matrix interleaver, the de-interleaver is an interleaver that exchange the original row and col
 * @param parallelFactor bits number per cycle, determines the throughput
 * @param forward
 */
case class Interleaver(row: Int, col: Int, parallelFactor: Int) extends Component {

  val running = in Bool()
  val we = in Bool()

  val dataIn = in Bits (parallelFactor bits)
  val dataOut = out Bits (parallelFactor bits)

  require(
    Seq(parallelFactor, row, col).forall(isPow2(_)) && // this can be removed if shifter works
      parallelFactor >= (row max col) &&
      parallelFactor % row == 0 &&
      parallelFactor % col == 0 &&
      (row * col) % parallelFactor == 0)

  val packSize = (parallelFactor / row) * (parallelFactor / col) // (intersection size of input and output)
  def packType = Bits(packSize bits)
  val squareSize = parallelFactor / packSize

  val rams = Seq.fill(squareSize)(Mem(packType, squareSize))
  val count = Counter(squareSize, inc = running)

  // wiring dataIn
  val dataInRearranged = cloneOf(dataIn)
  (0 until parallelFactor).foreach { i =>
    val bitId = parallelFactor - 1 - i
    val mapped = (i % (parallelFactor / col)) * col + i / (parallelFactor / col)
    val mappedBitId = parallelFactor - 1 - mapped
    dataInRearranged(bitId) := dataIn(mappedBitId)
  }
  val dataInShifted: Bits = dataInRearranged.rotateRight(count.value << log2Up(packSize))
  val dataInPacked = dataInShifted.subdivideIn(squareSize slices).reverse // dataInPacked(0) holds the MSBs

  val dataUnpacked = Bits(parallelFactor bits)
  dataUnpacked.clearAll()

  // square interleave
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

  val dataOutShifted = dataUnpacked.rotateLeft(count.value << 2)

  // wiring dataOut
  (0 until parallelFactor).foreach { i =>
    val bitId = parallelFactor - 1 - i

    val packId = i / packSize
    val rowId = packId * (parallelFactor / col) + i % (parallelFactor / col)
    val colId = i % packSize / (parallelFactor / col)

    val mapped = colId * row + rowId
    val mappedBitId = parallelFactor - 1 - mapped
    dataOut(mappedBitId) := dataOutShifted(bitId)
  }
}

object Interleaver extends App {
    VivadoSynth(new Interleaver(32,128,128), name = "Interleaver")
    VivadoSynth(new Interleaver(128,32,128), name = "DeInterleaver")
}
