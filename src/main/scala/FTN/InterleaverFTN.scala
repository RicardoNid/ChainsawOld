package FTN

import Chainsaw._
import spinal.core._
import spinal.lib._

/** High-throughput interlever that implements matrix interleaving
 *
 * @param row the semantic is the same as row of Matlab matintrlv
 * @param col the semantic is the same as row of Matlab matintrlv
 *            for matrix interleaver, the de-interleaver is an interleaver that exchange the original row and col
 * @param pF  bits number per cycle, determines the throughput
 */

case class InterleaverFTN(row: Int, col: Int, pF: Int) extends Component {

  val core = DSP.interleave.MatIntrlv(row, col, pF, pF, Bool())
  val dataIn = slave Flow Fragment(Bits(pF bits))
  val dataOut = master Flow Fragment(Bits(pF bits))

  core.dataIn.valid := dataIn.valid
  core.dataIn.payload := Vec(dataIn.fragment.asBools.reverse)

  core.dataOut.ready := True
  dataOut.valid := core.dataOut.valid
  dataOut.fragment := core.dataOut.payload.reverse.asBits()

  dataOut.last := Delay(dataIn.last, core.latency, init = False)
}

object InterleaverFTN extends App {
  VivadoSynth(new InterleaverFTN(32, 128, 256), name = "Interleaver")
  //  VivadoSynth(new InterleaverFTN(128, 32, 128), name = "DeInterleaver")
}
