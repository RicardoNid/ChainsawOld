package Chainsaw.fastAlgo

import Chainsaw.dspTest._
import cc.redberry.rings.scaladsl.UnivariateRingZp64
import spinal.core._
import spinal.lib._

/**
 *
 */
object huaweiNTT {

  val p = 3329 // 3329 = 13 * 256 + 1
  implicit val polyRing = UnivariateRingZp64(p, "x")
  implicit val cfRing = polyRing.cfRing

  def kred(c: IntegerRange) = {
    val cl = c % 256
    val ch = c / 256
    val cPrime = cl * 13 - ch

    val cPrimel = cPrime % 256
    val cPrimeh = cPrime / 256
    cPrimel * 13 - cPrimeh
  }

  val basic = IntegerRange(0, 4095)

  def correction(base: IntegerRange, possible: IntegerRange, modulo: Int) = {
    val lowGap = base.low - possible.low
    val addCorrection = if (lowGap > 0) lowGap / modulo else 0
    val highGap = possible.high - base.high
    val miusCorrection = if (highGap > 0) highGap / modulo else 0
    addCorrection + miusCorrection
  }

  def k2RED(dataIn: UInt) = {
    // step 1
    val cl = dataIn(7 downto 0)
    cl.setName("cl", weak = true)
    val ch = dataIn(23 downto 8)
    ch.setName("ch", weak = true)
    val cPrime = RegNext((13 * cl).intoSInt -^ ch.intoSInt) // 16 + 1 bits TODO: implement this by shift-add?
    cPrime.setName("cPrime", weak = true)
    // step 2
    val cPrimel = cPrime(7 downto 0).asUInt
    cPrimel.setName("cPrimel", weak = true)
    val cPrimeh = cPrime(16 downto 8)
    cPrimeh.setName("cPrimeh", weak = true)
    val CPrime2 = (13 * cPrimel).intoSInt - cPrimeh // 12 + 1 bits

    // do correction
    val (lower, higher) = (CPrime2 - 3329, CPrime2 + 3329)

    val ret = UInt(12 bits)
    when(CPrime2 < 0)(ret := higher.asUInt.resized)
      .elsewhen(lower >= 0)(ret := lower.asUInt.resized)
      .otherwise(ret := CPrime2.asUInt.resized)
    RegNext(ret)
  }

  def kMultMod(a: UInt, b: UInt) = {
    val prod = a * b
    prod.addAttribute("use_dsp", "no")
    RegNext(k2RED(prod))
  }

  import Chainsaw.DFG._
  val kMultModNode = BinaryNode(kMultMod, "kMultMod", delay = 3 cycles)

  def kAddMod(a: UInt, b: UInt) = {
    val (aS, bS) = (a.intoSInt, b.intoSInt)
    val sum = aS +^ bS
    val corrected = sum - 3329
    RegNext(Mux(corrected >= 0, corrected, sum).asUInt.resize(12 bits))
  }

  def kSubMod(a: UInt, b: UInt) = {
    val (aS, bS) = (a.intoSInt, b.intoSInt)
    val diff = aS - bS
    val corrected = diff + 3329
    RegNext(Mux(diff >= 0, diff, corrected).asUInt.resize(12 bits))
  }

  /** kred(k^-2^v * k^-2^w) -> k^-2vw^
   */
  def CTButterfly(u: UInt, v: UInt, omega: UInt): (UInt, UInt) = { // implement
    val vw = kMultMod(v, omega) // TODO: optimization?
    val uDelayed = Delay(u, 3)
    vw.setName("vw")
    (kAddMod(uDelayed, vw), kSubMod(uDelayed, vw))
  }

  def CTBF(u: UInt, v: UInt, omega: Int): (UInt, UInt) = CTButterfly(u,v,U(omega, 12 bits))


  def GSButterfly(u: UInt, v: UInt, omega: Int) = {
    (Delay(kAddMod(u, v), 3), kMultMod(kSubMod(u, v), U(omega, 12 bits)))
  }

}

case class K2REDHard() extends Component with DSPTestable[UInt, UInt] {
  override val dataIn: Flow[UInt] = slave Flow UInt(24 bits)
  override val dataOut: Flow[UInt] = master Flow UInt(12 bits)
  override val latency: Int = 2

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
  dataOut.payload := huaweiNTT.k2RED(dataIn.payload)
}

case class CTBFHard(omega: Int) extends Component with DSPTestable[Vec[UInt], Vec[UInt]] {

  require(omega >= 0 && omega <= 3329)

  override val dataIn: Flow[Vec[UInt]] = slave Flow Vec(UInt(12 bits), 2)
  override val dataOut: Flow[Vec[UInt]] = master Flow Vec(UInt(12 bits), 2)
  override val latency: Int = 4

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
  val ret = huaweiNTT.CTBF(dataIn.payload(0), dataIn.payload(1), omega)
  dataOut.payload := Vec(ret._1, ret._2)
}

case class GSBFHard(omega: Int) extends Component with DSPTestable[Vec[UInt], Vec[UInt]] {

  require(omega >= 0 && omega <= 3329)

  override val dataIn: Flow[Vec[UInt]] = slave Flow Vec(UInt(12 bits), 2)
  override val dataOut: Flow[Vec[UInt]] = master Flow Vec(UInt(12 bits), 2)
  override val latency: Int = 4

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
  val ret = huaweiNTT.GSButterfly(dataIn.payload(0), dataIn.payload(1), omega)
  dataOut.payload := Vec(ret._1, ret._2)
}