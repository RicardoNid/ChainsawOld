package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

class BigMem[T <: Data](wordType: HardType[Vec[T]], wordCount: Int) {

  val bitWidth = wordType().getBitsWidth
  val subMemNum = nextPower2(ceil(bitWidth / 4096.0)).toInt
  val vecWidth = wordType().length
  require(vecWidth % subMemNum == 0, s"vec width $vecWidth, sub mem number $subMemNum")
  val subVecWidth = vecWidth / subMemNum

  val elementType: HardType[T] = HardType(wordType().head)

  val subMems: Seq[Mem[Vec[T]]] = Seq.fill(subMemNum)(Mem(Vec(elementType, subVecWidth), wordCount))

  def divide(in: Vec[T]) = in.grouped(subVecWidth).toSeq.map(Vec(_))

  def combine(ins: Seq[Vec[T]]) = Vec(ins.flatten)

  def readAsync(address: UInt, readUnderWrite: ReadUnderWritePolicy = dontCare)
  = combine(subMems.map(mem => mem.readAsync(address, readUnderWrite)))

  def readSync(address: UInt, enable: Bool = null, readUnderWrite: ReadUnderWritePolicy = dontCare, clockCrossing: Boolean = false)
  = combine(subMems.map(mem => mem.readSync(address, enable, readUnderWrite, clockCrossing)))

  def write(address: UInt, data: Vec[T], enable: Bool = null, mask: Bits = null): Unit = {
    val divided = divide(data)
    subMems.zip(divided).foreach { case (mem, segment) => mem.write(address, segment, enable) }
  }

  def init(initContent: Seq[Vec[T]]) = {
    val divided = initContent.map(divide)
    subMems.indices.foreach { i =>
      subMems(i).init(divided.map(_.apply(i)))
    }
  }

}

object BigMem {
  def apply[T <: Data](wordType: HardType[Vec[T]], wordCount: Int): BigMem[T] = new BigMem(wordType, wordCount)

  def apply[T <: Data](initContent: Seq[Vec[T]]): BigMem[T] = {
    val ret = new BigMem(initContent(0), initContent.length)
    ret.init(initContent)
    ret
  }
}
