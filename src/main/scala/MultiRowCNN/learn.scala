package MultiRowCNN

import spinal.core._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

class CarryAdder(size: Int) extends Component {
  val a = in SFix(5 exp,-3 exp)
  val b = in SFix(5 exp,-3 exp)
  val result = out SFix(5 exp,-3 exp)

  result := a + b
}



