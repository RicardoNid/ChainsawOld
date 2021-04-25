package DSP

import spinal.core._
import spinal.lib.{Delay, master, slave}

case class ModularMultiplicationTestCase(A: Int, B: Int, N: Int)

/** Kernel for modular multiplication by Montgomery
 *
 * @param N modulo
 */
class ModularMultiplicationSim(N: Int) extends Component {
  val input = slave Flow (Vec(UInt(log2Up(N - 1) bits), 2))
  val output = master Flow UInt(log2Up(N - 1) bits)

  val mm = new ModularMultiplication(input.payload(0), input.payload(1), N)
  output.payload := mm.implicitValue
  val timing: TimingInfo = mm.getTimingInfo
  output.valid := Delay(input.valid, timing.latency, False)
}

object ModularMultiplicationSim {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new ModularMultiplicationSim(13))
  }
}




