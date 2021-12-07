package Chainsaw.examples

import Chainsaw._
import spinal.core._

// Vivado could figure this out, but still, it influence the verilog quality
object ROMMUXExample {

  def main(args: Array[String]): Unit = {
    def data: Seq[UInt] = (0 until 128).map(_ => ChainsawRand.nextBigInt(12)).map(U(_, 12 bits))

    VivadoSynth(new Component {

      val counterIn = in UInt (7 bits)
      val dataOut = out UInt (12 bits)

      switch(counterIn) {
        (0 until 128).foreach { i =>
          is(U(i, 7 bits))(dataOut := data(i))
        }
      }

    }, name = "ROMasMUX")

    VivadoSynth(new Component {

      val counterIn = in UInt (7 bits)
      val dataOut = out UInt (12 bits)
      val ROM = Mem(data)

      dataOut := ROM.readAsync(counterIn)

    }, name = "ROMasROM")
  }

}
