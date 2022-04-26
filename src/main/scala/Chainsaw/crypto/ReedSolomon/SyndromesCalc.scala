package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class SyndromesCalc(n: Int, k: Int) extends Component {
  val io = new Bundle {
    val r = slave Flow (UInt(n - k bits))
    val s = master Flow (Vec(UInt(n - k bits), n - k))
  }
  val reload = Bool()
  val S      = Range(0, n - k).map(i => HornerPloy(i, n, k, io.r, reload))
  S.zip(io.s.payload).foreach { case (syn, p) => p := syn.reg }

  val counter = Counter(0, n)
  when(io.r.valid)(counter.increment())
  when(counter.willOverflowIfInc)(counter.clear())
  reload     := counter.willOverflowIfInc
  io.s.valid := counter.willOverflowIfInc
}
