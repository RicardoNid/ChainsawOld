package Chainsaw.crypto.RSA

import spinal.core._

class MontExpRouter[T <: BitVector](config: MontConfig, hardType: HardType[T], NTo1: Boolean = true) extends Component {

  import config._

  val connected = in Bool
  val mode = in Bits (lMs.size bits)
  val selectCount = in UInt (log2Up(parallelFactor) bits)
  val src = in Vec(hardType, parallelFactor)
  val toDes = out Vec(hardType, parallelFactor)
  toDes.foreach(_.clearAll()) // pre-assign

  switch(True) { // for different modes
    lMs.indices.foreach { modeId => // traverse each mode, as each mode run instances of different size lM
      val starterIds = startersAtModes(modeId)
      if (NTo1) is(mode(modeId))(when(connected)(starterIds.foreach(j => toDes(j) := src(selectCount + j))))
      else is(mode(modeId))(when(connected)(starterIds.foreach(j => toDes(selectCount + j) := src(j)))) // selection network version
    }
  }

  def preClear() = {
    connected.clear()
    mode.clearAll()
    selectCount.clearAll()
    src.foreach(_.clearAll())
    connected.allowOverride
    mode.allowOverride
    selectCount.allowOverride
    src.allowOverride
  }
}

object MontExpRouter {
  def apply[T <: BitVector](config: MontConfig, hardType: HardType[T], NTo1: Boolean = true): MontExpRouter[T] = {
    val ret = new MontExpRouter(config, hardType, NTo1)
    ret.preClear()
    ret
  }
}