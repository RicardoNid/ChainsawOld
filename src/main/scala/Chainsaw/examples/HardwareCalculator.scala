package Chainsaw.examples

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

case class HardwareCalculator()  extends Component with DSPTestable [Vec[UInt], UInt]{
  override val dataIn: Flow[Vec[UInt]] = slave Flow Vec(UInt(8 bits), 2)
  override val dataOut: Flow[UInt] = master Flow UInt(8 bits)

  dataOut.payload := RegNext(dataIn.payload(0) + dataIn.payload(1))
  override val latency: Int = 1
  dataOut.valid := Delay(dataIn.valid, latency)
}

object HardwareCalculator {
  def main(args: Array[String]): Unit = {
    val ret = doFlowPeekPokeTest("testAdder", HardwareCalculator(), Seq(Seq(115,125).map(BigInt(_))), Seq(BigInt(240)))
    println(ret.mkString(" "))
   VivadoSynth(HardwareCalculator())
  }
}

