package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class LUTRAM extends Component {

  val counter = CounterFreeRun(42)
  val rams = Seq.fill(42)(Mem(Bits(1 bits), 64))

  val input = in Bits (1 bits)
  val addr = in UInt (6 bits)
  val output = out (CombInit(B"0"))

  switch(counter.value) {
    (0 until 42).foreach { i =>
      is(U(i)) {
        rams(i)(addr) := input
        output := rams(i)(addr + 1)
      }
    }
  }
}

object LUTRAM {
  def main(args: Array[String]): Unit = {
    VivadoSynth(new LUTRAM)
  }
}
