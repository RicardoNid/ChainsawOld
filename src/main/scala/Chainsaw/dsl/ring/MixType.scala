package Chainsaw.dsl.ring

import spinal.core._
import scala.util.Random

import scala.reflect.{ClassTag, classTag}

abstract class MixType[T: ClassTag] {

  val tag = classTag[T]

  val width: Int

  def toBits(value: T): String

  def fromBits(bits: String): T

  def toBigInt(value: T) = BigInt(toBits(value), 2)

  def fromBigInt(bigInt: BigInt) = fromBits(bigInt.toString(2).reverse.padTo(width, '0').reverse)

  def undefined = Bits(width bits)

  def toCoeff(coeff: T) = B(toBigInt(coeff), width bits)

  def selfTest() = {
    def randBits = (0 until width).map(_ => Random.nextInt(2)).map(_.toString).mkString("")

    val stimulus = (0 until 100).map(_ => randBits)
    stimulus.foreach { stimuli =>
      assert(toBits(fromBits(stimuli)) == stimuli)
    }
  }
}
