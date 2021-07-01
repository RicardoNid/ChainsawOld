package Chainsaw.Crypto.RSA.history

import Chainsaw._
import spinal.core._
import spinal.lib._

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
