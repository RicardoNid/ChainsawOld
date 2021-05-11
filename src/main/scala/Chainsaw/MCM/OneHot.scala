package Chainsaw.MCM

import spinal.core._

class OneHot extends Component {

  val input = in Bits (3 bits)

  val ready = B"0"
  val set = B"0"
  val going = B"0"

  switch(input) {
    is(B"001")(ready := B"1")
    is(B"010")(set := B"1")
    is(B"100")(going := B"1")
  }

}
object OneHot {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new OneHot)
  }
}


