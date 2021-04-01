package sysu.util

import spinal.core._

class SeqGenModule(sequence: Seq[Int], mode:SeqGenMode) extends Component {
  val seqgen = SeqGen(sequence, mode)

  val io = new Bundle{
    val output = out UInt(seqgen.implicitValue.getBitsWidth bits)
  }

  io.output := seqgen
}
