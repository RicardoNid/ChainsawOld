package Chainsaw.Crypto.RSA

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class MontExpRouter[T <: BitVector](config: MontConfig, hardType: HardType[T]) extends Component {

  import config._

  val work = in Bool
  val mode = in Bits(lMs.size bits)
  val selectCount = in UInt (log2Up(parallelFactor) bits)
  val src = in Vec(hardType, parallelFactor)
  val toDes = out Vec(hardType, parallelFactor)

  toDes.foreach(_.clearAll())

  switch(True) { // for different modes
    lMs.indices.foreach { modeId => // traverse each mode, as each mode run instances of different size lM
      val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(modeId) == 0) // instance indices of current mode
        .take(parallelFactor / groupPerInstance(modeId))
      is(mode(modeId)) {
        starterIds.foreach(j => toDes(j) := src(selectCount + j)) // selection network version
      }
    }
  }
}
