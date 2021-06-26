package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class MontExpRouter[T <: Data](parallelFactor:Int, hardType: HardType[T]) extends Component {

  val src = in Vec(hardType, parallelFactor)
  val des: out Vec[T], selectCount: UInt

  switch(True) { // for different modes
    lMs.indices.foreach { modeId => // traverse each mode, as each mode run instances of different size lM
      val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(modeId) == 0) // instance indices of current mode
        .take(parallelFactor / groupPerInstance(modeId))
      is(modeReg(modeId)) {
        starterIds.foreach(j => des(j) := src(selectCount + j)) // selection network version
      }
    }
  }

}
