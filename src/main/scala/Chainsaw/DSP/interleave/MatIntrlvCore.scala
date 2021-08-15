package Chainsaw.DSP.interleave

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

case class MatIntrlvCore[T <: Data](row: Int, col: Int, dataType: () => T, vec:Boolean) extends Component {

  val dataIn = in Vec(dataType(), row)
  val dataOut = out Vec(dataType(), col)
  val shift = in UInt(3 bits)

  def vecRotateLeft[T <: Data](dataIn: Vec[T], that: UInt): Vec[T] = {
    val hardType = HardType(dataIn(0))
    val row = dataIn.size
    val col = dataIn(0).getBitsWidth
    val allBits = dataIn.map(_.asBits.asBools)
    val transposed = Algos.matIntrlv2D2D(allBits, row, col)
    val rotated = transposed.map(_.asBits().rotateLeft(that).asBools)
    val transposedBack = Algos.matIntrlv2D2D(rotated, col, row).map(_.asBits())
    val ret = Vec(hardType, row)
    ret.zip(transposedBack).foreach { case (t, bits) => t.assignFromBits(bits) }
    ret
  }

  def rotateLeft[T <: Data](dataIn:Vec[T], that:UInt) = dataOut.assignFromBits(dataIn.asBits.rotateLeft(that * widthOf(dataIn.dataType))) // seems that Xilinx synth can figure this out

  if(vec) dataOut := vecRotateLeft(dataIn, shift) else dataOut.assignFromBits(dataIn.rotateLeft(shift))  // seems that Xilinx synth can figure this out
}

object MatIntrlvCore extends App {
  def dataType() = UInt(8 bits)
  VivadoSynth(new MatIntrlvCore(10, 10, dataType, vec = false))
  VivadoSynth(new MatIntrlvCore(10, 10, dataType, vec = true))
}
