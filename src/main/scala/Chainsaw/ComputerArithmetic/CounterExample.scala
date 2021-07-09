package Chainsaw.ComputerArithmetic

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class CounterExample(cascaded: Boolean) extends Component {
  val ret = out UInt (12 bits)
  if (cascaded) {
    val count0 = CounterFreeRun(16) // 2^4
    val count1 = Counter(16, inc = count0.willOverflow) // 2^4
    val count2 = Counter(16, inc = count1.willOverflow) // 2^4
    ret := count2.value @@ count1.value @@ count0.value
  }
  else{
    val count = CounterFreeRun(1 << 12)
    ret := count.value
  }
}

object CounterExample {
  def main(args: Array[String]): Unit = {
    VivadoSynth(new CounterExample(cascaded = true), name = "cascaded_counter")
    VivadoSynth(new CounterExample(cascaded = false), name = "whole_counter")
  }
}
