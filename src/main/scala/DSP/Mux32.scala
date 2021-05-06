package DSP

import spinal.core._

class Mux32 extends Component {

  val sel = in UInt (5 bits)
  val contents = in Bits (32 bits)
  val output = out Bool()

  switch(sel) {
    (0 until 32).foreach { i =>
      is(U(i, 5 bits))(output := contents(i))
    }
  }
}

object Mux32 {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new Mux32)
  }
}
