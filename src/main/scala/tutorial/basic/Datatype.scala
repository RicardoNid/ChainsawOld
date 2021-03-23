package tutorial.basic

import spinal.core._

class Datatype extends Component {

//  def getGray(zeroBasedValue: Int) = {
//    val value = zeroBasedValue + 1
//    if (log2Up(value) == 1) {
//      if (zeroBasedValue == 0) 0
//      else 1
//    }
//    else {
//      if value
//    }
//  }

  object myState extends SpinalEnum{

    val e0, e1, e2, e3 = newElement()
    defaultEncoding = SpinalEnumEncoding("dynamicEncoding", _ * 2 + 1)
    defaultEncoding = native
  }

  val io = new Bundle {

    val output = out Bits(2 bits)
  }

  io.output := myState.e3.asBits
}

object Datatype {
  def main(args: Array[String]): Unit = {
    SpinalConfig(targetDirectory = "output/VerilogLike")
      .generateSystemVerilog(new Datatype)
  }
}
