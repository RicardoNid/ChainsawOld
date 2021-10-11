package Chainsaw.DSP.FFT

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import Chainsaw.DFG._

// build fft as a graph
class DFGFFT {

  val butterfly = (dataIns: Seq[ComplexNumber], globalCount: GlobalCount) => {
    val add = dataIns(0) + dataIns(1)
    val sub = dataIns(0) - dataIns(1)
    Seq(add, sub)
  }

  val fft4 = DFGGraph[ComplexNumber]

  import Operators._

  val butterflyHardware = DSPHardware(impl = butterfly, inDegree = 2, outWidths = Seq(-1 bits, -1 bits))
  val bs = (0 until 4).map(i => butterflyHardware.asDSPNode(s"b$i", 1 cycles, 1 ns))
  bs.foreach(fft4.addVertex(_))

  val edge0 = DefaultDelay[ComplexNumber](0, 0)
  val edge1 = DefaultDelay[ComplexNumber](0, 1)
  val edge2 = DefaultDelay[ComplexNumber](1, 0)
  val edge3 = DefaultDelay[ComplexNumber](1, 1)

  fft4.addEdge(bs(0), bs(2), edge0)
  fft4.addEdge(bs(0), bs(3), edge2)
  fft4.addEdge(bs(1), bs(2), edge1)
  fft4.addEdge(bs(1), bs(3), edge3)

  fft4.setInput(bs(0), 0)
  fft4.setInput(bs(0), 1)
  fft4.setInput(bs(1), 0)
  fft4.setInput(bs(1), 1)

  fft4.setOutput(bs(2), 0)
  fft4.setOutput(bs(2), 1)
  fft4.setOutput(bs(3), 0)
  fft4.setOutput(bs(3), 1)
}

object DFGFFT {
  def main(args: Array[String]): Unit = {

    println(new DFGFFT().fft4)

    GenRTL(new Component {
      val dataIns = in(Vec(ComplexNumber(1, -6), 4))
      val dataOuts = out(Vec(ComplexNumber(1, -6), 4))
      dataOuts := Vec(new DFGFFT().fft4.implForwarding(dataIns))
    })
  }
}
