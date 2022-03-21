package Chainsaw.examples

import spinal.core._
import Chainsaw._

case class inferringDSP() extends Component {
  val dataWidth  = 16
  val A, B, C, D = in SInt (dataWidth bits)
  val ret        = out SInt (2 * dataWidth + 1 bits)

  //  ret := (A +^ D) * B + C // using +^ for the pre adder is necessary, otherwise, it won't be absorbed in DSP block
  ret := RegNext(RegNext(RegNext(A +^ D) * B) + C) // using +^ for the pre adder is necessary, otherwise, it won't be absorbed in DSP block
}

object inferringDSP {
  def main(args: Array[String]): Unit = {
    VivadoSynth(inferringDSP())
  }
}
