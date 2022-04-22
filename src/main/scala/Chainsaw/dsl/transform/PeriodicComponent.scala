package Chainsaw.dsl.transform

import spinal.core._
import spinal.lib._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class PeriodicComponent(transform: (Vec[Bits], Bool) => Vec[Bits],
                        inputSize: Int, inputWidth: Int,
                        outputSize: Int, outputWidth: Int, latency: Int) extends Component() {

  val dataIn = in(Fragment(Vec(Bits(inputWidth bits), inputSize)))
  val dataOut = out(Fragment(Vec(Bits(outputWidth bits), outputSize)))

  dataOut.fragment := transform(dataIn.fragment, dataIn.last)
  dataOut.last := Delay(dataIn.last, latency, init = False)
}

object PeriodicComponent {
  def main(args: Array[String]): Unit = {

    def lut(index: Vec[Bits], last: Bool) = {
      val ROM = Mem((0 until 8).map(U(_, 3 bits)))
      val counter = CounterFreeRun(8)
      when(last)(counter.clear())
      Vec(ROM.readAsync(index.head.asUInt).asBits)
    }

    GenRTL(new PeriodicComponent(lut, 1, 3, 1, 3, 0))
  }
}


