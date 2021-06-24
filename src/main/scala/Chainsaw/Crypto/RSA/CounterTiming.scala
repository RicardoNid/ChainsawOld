package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class CounterTiming(cascaded: Boolean = true) extends Component {

  val output = out(UInt(12 bits))

  if (cascaded) {
    val count1 = CounterFreeRun(16)
    val count2 = Counter(16, inc = count1.willOverflow)
    val count3 = Counter(16, inc = count2.willOverflow)
    output := (count3.value @@ count2.value @@ count1.value)
  }else{
    val count = CounterFreeRun(4096)
    output := count.value
  }

}

object CounterTiming {
  def main(args: Array[String]): Unit = {
    VivadoSynth(new CounterTiming(true))
    VivadoSynth(new CounterTiming(false))
  }
}
