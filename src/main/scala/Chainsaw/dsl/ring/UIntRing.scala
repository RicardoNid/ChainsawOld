package Chainsaw.dsl.ring

import spinal.core.Bits
import Chainsaw.dsl._

class UIntRing(val width:Int) extends MixType[Int] with RingOp[Int] {

  override def toBits(value: Int) = BigInt(value).toUnsigned(width)

  override def fromBits(bits: String) = bits.asUnsigned.toInt

  override def add(a: Int, b: Int) = a + b

  override def sub(a: Int, b: Int) = a - b

  override def mult(a: Int, b: Int) = a * b

  override val addH = new HardOp2(
    op = (a: Bits, b: Bits) => (a.asUInt +^ b.asUInt).resize(width).asBits,
    latency = 0
  )

  override val subH = new HardOp2(
    op = (a: Bits, b: Bits) => (a.asUInt -^ b.asUInt).resize(width).asBits,
    latency = 0
  )

  override val multH = new HardOp2(
    op = (a: Bits, b: Bits) => (a.asUInt * b.asUInt).resize(width).asBits,
    latency = 0
  )
}

object UIntRing {
  def apply(width:Int): UIntRing = new UIntRing(width)
}
