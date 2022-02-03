package Chainsaw.crypto

import Chainsaw._
import spinal.core._


/** linear feedback shift registers
 *
 * @param poly characteristic polynomial of lfsr, represented by a sequence of 0s and 1s with a leading 1 and an ending 1
 * @param vec  when set, using all registers as outputs, rather than the last register(as definition)
 * @see [[https://www.eng.auburn.edu/~strouce/class/elec6250/LFSRs.pdf]]
 * @example Seq(1,1,0,1,1) means x^4^ + x^3^ + x + 1, the implementation contains 4 FFs and 2 XORs
 */
case class Lfsr(poly: Seq[Int], vec: Boolean = false) extends Component {
  require(poly.head == 1 && poly.last == 1 && poly.forall(i => i >= 0 && i <= 1), "poly is a sequence of 0s and 1s with a leading 1 and an ending 0")

  val guide = poly.init.reverse // using this as a guide to build lfsr of internal type

  val lastReg = RegInit(True) // the last reg(highest order term in the polynomial) is special
  lastReg.addAttribute("max_fanout", 50)

  val inits = guide.init.map { i =>
    val head = Reg(Bool())
    val lasts = if (i == 1) (head ^ lastReg) else head
    (head, lasts)
  }
  val heads: Seq[Bool] = inits.map(_._1)
  val lasts = inits.map(_._2)

  // connecting
  heads.head := lastReg
  heads.tail.zip(lasts.init).foreach { case (head, last) => head := last }
  lastReg := lasts.last

  val dataOut =  if (vec) out(Vec(heads :+ lastReg).asBits) else out(lastReg.asBits)
}

object Lfsr extends App {
  //  GenRTL(Lfsr(Seq(1, 1, 0, 1, 1)))
  //  VivadoSynth(Lfsr(Seq(1, 1, 0, 1, 1)))
  //  VivadoSynth(Lfsr(Seq.fill(500)(1)))
  VivadoSynth(Lfsr(Seq.fill(500)(1)))
}
