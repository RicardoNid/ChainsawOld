package xilinx.dsp48e2

import Chainsaw._
import spinal.core._
import spinal.lib._

class DSPExample1 extends Component {
//  val a = in Vec(SInt(27 bits), 5)
//  val b = in Vec(SInt(18 bits), 5)
//  val mults = (0 to 4).map(i => out(Delay(a(i) * b(i), i)))

  // USE_MULT = DYNAMIC
  // TODO: implement this by pure DSP
  //  val sel = in Bool()
  //  val c = in Vec(SInt(27 bits), 4)
  //  val d = in Vec(SInt(18 bits), 4)
  //
  //  val dynamics = (0 to 3).map { i =>
  //    val mult = c(i) * d(i)
  //    val add = c(i) + d(i)
  //    out(Delay(Mux(sel, mult, add), i))
  //  }
  //  dynamics.foreach(_.addAttribute("use_dsp", "yes"))

  // USE_MULT = NONE
  // TODO: implement latency = 2 by pure DSP
  //  val e = in Vec(SInt(47 bits), 4)
  //  val f = in Vec(SInt(47 bits), 4)
  //  val nones = (0 to 2).map(i => out(Delay(e(i) +^ f(i), i)))
  //  nones.foreach(_.addAttribute("use_dsp","yes"))

  // counter mode
  val counter = CounterFreeRun((BigInt(1) << 32) - 1)
  counter.value.addAttribute("use_dsp", "yes")
  out(counter.value)
}

object DSPExample1 {
  def main(args: Array[String]): Unit = {
    VivadoSynth(new DSPExample1)
  }
}
