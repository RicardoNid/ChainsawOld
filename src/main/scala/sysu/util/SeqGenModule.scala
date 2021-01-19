package sysu.util

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import sysu.xilinx._
import sysu.util._
import sysu.CNN._

class SeqGenModule(sequence: Seq[Int], mode:SeqGenMode) extends Component {
  val seqgen = SeqGen(sequence, mode)

  val io = new Bundle{
    val output = out UInt(seqgen.implicitValue.getBitsWidth bits)
  }

  io.output := seqgen
}
