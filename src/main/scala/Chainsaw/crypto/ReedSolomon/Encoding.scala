package Chainsaw.crypto.ReedSolomon

import spinal.core._
import spinal.lib._

/** RS code (n, k)
 * @param n
 *   length of the code word
 * @param k
 *   number of message symbols
 * @param m
 *   bit width of symbol
 */
case class Encoding(n: Int, k: Int, m: Int) extends Component {
  val io = new Bundle {
    val input   = in UInt (m bits)
    val control = in Bool ()
    val output  = out UInt (m bits)
  }

  val regs = Vec(Reg(UInt(m bits)) init(0), n - k)
  import GaloisFieldHardWare._
  val coff     = new RS(n, k).getGxCoff
  val loadData = Mux(io.control, io.input.add(regs.head), U(0))
  val multiRes = coff.tail.map(loadData.multi(_))

  regs.init.zip(regs.tail).zip(multiRes.init).foreach { case ((r, rn), m) => r := rn.add(m) }
  regs.last := multiRes.last
  io.output := Mux(io.control, regs.head, io.input)
}

