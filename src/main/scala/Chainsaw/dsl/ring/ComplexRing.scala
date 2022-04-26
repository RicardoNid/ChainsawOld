package Chainsaw.dsl.ring

import Chainsaw.ComplexMult
import Chainsaw.ComplexNumber
import Chainsaw.dsl._
import breeze.math._
import spinal.core.{Bits, _}

class ComplexRing(integral: Int, fractional: Int) extends MixType[Complex] with RingOp[Complex] {
  override val width = (integral + fractional + 1) * 2

  val ct = HardType(ComplexNumber(integral, -fractional))
  val st = HardType(SFix(integral exp, -fractional exp))

  def scaling = (1 << fractional).toDouble

  override def toBits(value: Complex) = {
    val toInt: Double => BigInt = x => BigInt((x * scaling).toInt)
    toInt(value.imag).toSigned(width / 2) ++ toInt(value.real).toSigned(width / 2)
  }

  override def fromBits(bits: String) = {
    val (imag, real) = bits.splitAt(width / 2)
    Complex(real.asSigned.toDouble / scaling, imag.asSigned.toDouble / scaling)
  }

  override def add(a: Complex, b: Complex) = a + b

  override def sub(a: Complex, b: Complex) = a - b

  override def mult(a: Complex, b: Complex) = a * b

  // TODO: not implemented
  override val addH = new HardOp2(
    op = (a: Bits, b: Bits) => {
      val complexA, complexB = ct()
      complexA.assignFromBits(a)
      complexB.assignFromBits(b)
      (complexA + complexB).asBits
    },
    latency = 1
  )
  override val subH = new HardOp2(
    op = (a: Bits, b: Bits) => {
      val complexA, complexB = ct()
      complexA.assignFromBits(a)
      complexB.assignFromBits(b)
      (complexA - complexB).asBits
    },
    latency = 1
  )

  override val multH = new HardOp2(
    op = (a: Bits, b: Bits) => {
      val cmult = ComplexMult(ct, ct)(Chainsaw.ComplexMultConfig(true, 3))
      cmult.a.assignFromBits(a)
      cmult.b.assignFromBits(b)
      cmult.p.truncated(st).asBits
    },
    latency = 3
  )
}

object ComplexRing {
  def apply(integral: Int, fractional: Int): ComplexRing = new ComplexRing(integral, fractional)
}
