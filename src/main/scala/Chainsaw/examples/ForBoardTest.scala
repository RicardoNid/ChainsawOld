package Chainsaw.examples

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

case class ForBoardTest() extends Component {

  val couter = CounterFreeRun(1024)
  out(couter.value)

}

object ForBoardTest {
  def main(args: Array[String]): Unit = {
    GenRTL(ForBoardTest())
  }
}
