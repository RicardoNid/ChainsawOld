package MultiRowCNN

import spinal.core._

class CarryAdder(size: Int) extends Component {
  val io = new Bundle {
    val a = in UInt (size bits)
    val b = in UInt (size bits)
    val result = out UInt (size bits)
  }
  var c = False
  for (i <- 0 until size) {
    val x = io.a(i)
    val y = io.b(i)

    io.result(i) := x ^ y ^ c
    c \= (x & y) | (x & c) | (y & c);
  }
}

object RLT {
}