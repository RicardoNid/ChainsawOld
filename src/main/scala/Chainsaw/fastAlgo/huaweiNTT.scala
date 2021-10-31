package Chainsaw.fastAlgo

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import Chainsaw.DFG._

object huaweiNTT {
  def k2RED(dataIn: UInt) = {
    // step 1
    val cl = dataIn(7 downto 0)
    cl.setName("cl")
    val ch = dataIn(23 downto 8)
    ch.setName("ch")
    val cPrime = RegNext((13 * cl).intoSInt -^ ch.intoSInt) // 16 + 1 bits TODO: implement this by shift-add?
    cPrime.setName("cPrime")
    // step 2
    val cPrimel = cPrime(7 downto 0).asUInt
    cPrimel.setName("cPrimel")
    val cPrimeh = cPrime(16 downto 8)
    cPrimeh.setName("cPrimeh")
    val ret = (13 * cPrimel).intoSInt - cPrimeh // 12 + 1 bits
    RegNext(Mux(ret >= 0, ret.asUInt, (ret + 3329).asUInt).resize(12 bits)) // no sign bit
  }

  def kMultMod(dataIns: Vec[UInt]) = {
    val prod = dataIns(0) * dataIns(1)
    prod.addAttribute("use_dsp", "no")
    RegNext(k2RED(prod))
  }

  def kAddMod(dataIns: Vec[UInt]) = {
    val sum = k2RED(dataIns(0) * dataIns(1))
    val reduced = sum - 3329
    Mux(reduced >= 0, reduced, sum)
  }

  def CTButterfly(u: UInt, v: UInt) = {

  }

}

case class K2REDHard() extends Component with DSPTestable[UInt, UInt] {
  override val dataIn: Flow[UInt] = slave Flow UInt(24 bits)
  override val dataOut: Flow[UInt] = master Flow UInt(12 bits)
  override val latency: Int = 2

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
  dataOut.payload := huaweiNTT.k2RED(dataIn.payload)
}

case class KMultModHard() extends Component with DSPTestable[Vec[UInt], UInt] {
  override val dataIn: Flow[Vec[UInt]] = slave Flow Vec(UInt(12 bits), 2)
  override val dataOut: Flow[UInt] = master Flow UInt(12 bits)
  override val latency: Int = 3

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
  dataOut.payload := huaweiNTT.kMultMod(dataIn.payload)
}