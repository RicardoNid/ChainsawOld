package Chainsaw.crypto.lattice

import Chainsaw.DFG.FFTArch._
import Chainsaw.DFG._
import Chainsaw.{GenRTL, IntegerRange, printlnGreen}
import Chainsaw.crypto._
import cc.redberry.rings.scaladsl._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

object HuaweiKyber {

  // TEMP: RegNext should have no init value
  // huawei configurations p = p, polySize = 256
  val p = 3329 // p = 13 * 256 + 1
  val k = 13
  val k2: Int = k * k
  val polySize = 256 // is not the "256" above, the 256 above is 1 << 8, they're the same by coincident
  implicit val polyRing: UnivariateRingZp64 = UnivariateRingZp64(p, "x")
  implicit val cfRing: Ring[Long] = polyRing.cfRing

  // we have the corresponding software algos distributed in Chainsaw.crypto

  /** derive the range of k^2^ reduction from an input range
   *
   * @param c input range
   * @return range of direct result of k^2^ reduction
   */
  def getK2REDRange(c: IntegerRange): IntegerRange = {
    val cl = c % 256
    val ch = c / 256
    val cPrime = cl * 13 - ch

    val cPrimel = cPrime % 256
    val cPrimeh = cPrime / 256
    cPrimel * 13 - cPrimeh
  }

  // given input range [0, 3328 * 3328], the result range is [-12, ], which need two corrections // TODO: is it compact?


  /** given a number in the possible range, the number of correction we need to correct it to the base range
   */
  def correctionNeed(base: IntegerRange, possible: IntegerRange)(implicit ring: Ring[Long]): Long = {
    val modulo = ring.p
    val lowGap = base.low - possible.low
    val addCorrection = if (lowGap > 0) lowGap / modulo else 0
    val highGap = possible.high - base.high
    val miusCorrection = if (highGap > 0) highGap / modulo else 0
    addCorrection + miusCorrection
  }

  // following functions are the hardware implementation of modular multiplication

  /** hardware implementation of K2RED
   */
  def k2RED(dataIn: UInt) = {
    // step 1
    //    require(dataIn.getBitsWidth == 24)
    val cl = dataIn(7 downto 0)
    val ch = dataIn(23 downto 8)
    val cPrime: SInt = RegNext((13 * cl).intoSInt -^ ch.intoSInt) // 16 + 1 bits TODO: implement this by shift-add?
    cPrime.setName("cPrime", weak = true)
    // step 2
    val cPrimel = cPrime(7 downto 0).asUInt
    val cPrimeh = cPrime(16 downto 8)
    val CPrime2: SInt = RegNext((13 * cPrimel).intoSInt - cPrimeh) // 12 + 1 bits

    val ret = UInt(12 bits)
    // do correction
    val (lower, higher) = (CPrime2 - p, CPrime2 + p)
    when(CPrime2 < 0)(ret := higher.asUInt.resized)
      .elsewhen(lower >= 0)(ret := lower.asUInt.resized)
      .otherwise(ret := CPrime2.asUInt.resized)

    // for wavefile debugging
    Seq(cl, ch, cPrime, cPrimel, cPrimel, CPrime2)
      .zip(Seq("cl", "ch", "cPrime", "cPrimel", "cPrimel", "CPrime2"))
      .foreach { case (signal, str) => signal.setName(str, weak = true) }

    RegNext(ret)
  }

  /** multiplication + K2RED
   */
  def kMultMod(a: UInt, b: UInt): UInt = {
    val prod = (a * b).resize(24 bits)
    prod.addAttribute("use_dsp", "no")
    k2RED(RegNext(prod))
  }

  /** modular addition(with correction)
   */
  def kAddMod(a: UInt, b: UInt): UInt = {
    val (aS, bS) = (a.intoSInt, b.intoSInt)
    val sum = aS.resize(13 bits) +^ bS.resize(13 bits)
    val corrected = sum - p
    RegNext(Mux(corrected >= 0, corrected, sum).asUInt.resize(12 bits))
  }

  /** modular subtraction(with correction)
   */
  def kSubMod(a: UInt, b: UInt): UInt = {
    val (aS, bS) = (a.intoSInt, b.intoSInt)
    val diff = aS - bS
    val corrected = diff + p
    RegNext(Mux(diff >= 0, diff, corrected).asUInt.resize(12 bits))
  }

  // TODO: find the minimum correction in ct/gs butterfly

  /** kred(k^-2^v * k^-2^w) -> k^-2vw^
   */
  def CTButterfly(u: UInt, v: UInt, omega: UInt): (UInt, UInt) = { // implement
    val vw = kMultMod(v, omega)
    val uDelayed = Delay(u.resize(24 bits), 4)
    vw.setName("vw", weak = true)
    (kAddMod(uDelayed, vw), kSubMod(uDelayed, vw))
  }

  def CTBF(u: UInt, v: UInt, omega: Int): (UInt, UInt) = CTButterfly(u, v, U(omega, 12 bits))

  def GSButterfly(u: UInt, v: UInt, omega: UInt): (UInt, UInt) = {
    (Delay(kAddMod(u, v).resize(12 bits), 4), kMultMod(kSubMod(u, v).resize(12 bits), Delay(omega.resize(12 bits), 1)))
  }

  def GSBF(u: UInt, v: UInt, omega: Int): (UInt, UInt) = GSButterfly(u, v, U(omega, 12 bits))

  // encapsulate the hardware impl as DFG nodes
  val kMultModNode: BinaryNode[UInt] = BinaryNode(kMultMod, "kMultMod", delay = 4 cycles)
  val ctButterflyNode: ButterflyNode[UInt] = ButterflyNode(CTButterfly, "ctButterfly", delay = 5 cycles, width = 12 bits)
  val gsButterflyNode: ButterflyNode[UInt] = ButterflyNode(GSButterfly, "gsButterfly", delay = 5 cycles, width = 12 bits)

  // parameters for constructing the NTT DFG
  val N = 128
  val omega: Long = cfRing.getNthRoot(N)
  val k2Inverse: Long = cfRing.inverseOf(k2)
  val NInverse: Long = cfRing.inverseOf(N)
  val coeffGen: Int => Long = (index: Int) => cfRing(cfRing.pow(omega, index) * k2Inverse)
}

object runKyber {
  def main(args: Array[String]): Unit = {
    import HuaweiKyber.{ctButterflyNode, gsButterflyNode, coeffGen}
    implicit def long2UInt: (Long, BitCount) => UInt = (value: Long, _: BitCount) => U(value, 12 bits) // TODO:

    val nttDFG_folded_8: DFGGraph[UInt] = ButterflyGen(ctButterflyNode, gsButterflyNode, size = 16, DIF, inverse = false, coeffGen, 12 bits, -2).getGraph

    //    val nttDFG_folded_8: DFGGraph[UInt] = ButterflyGen(ctButterflyNode, gsButterflyNode, size = 128, DIF, inverse = false, coeffGen, 12 bits, -8).getGraph
    //    val nttDFG = ButterflyGen(ctButterflyNode, gsButterflyNode, size = 128, DIF, inverse = false, coeffGen, 12 bits, 1).getGraph

    //    genDSPNode(nttDFG_folded_8.asNode("ntt128_folded_8", 0 cycles), Seq.fill(128)(12 bits))
    //    synthDSPNode(nttDFG.asNode("ntt128_folded_8", 0 cycles), Seq.fill(128)(12 bits))
    //    synthDFG(nttDFG_folded_8, Seq.fill(128)(12 bits), forTiming = true)
  }
}
