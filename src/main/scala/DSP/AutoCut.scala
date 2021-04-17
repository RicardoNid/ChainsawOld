package DSP

import spinal.core._ //  for digital signal processing

class AutoCut(a: UInt, b: UInt, c: UInt) extends ImplicitArea[(UInt, UInt, UInt)] {
  val d = a + b
  val e = b + c
  val f = a + c

  override def implicitValue = (d, e, f)
}

object AutoCut {
  def apply(a: UInt, b: UInt, c: UInt): AutoCut = new AutoCut(a, b, c)
}

class AutoCutGen extends Component {
  val a = in UInt (4 bits)
  val b = in UInt (4 bits)
  val ret = out UInt (4 bits)
  ret := AutoCut(a, b, U(0)).implicitValue._1
}

object AutoCutGen {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new AutoCutGen)
  }
}
