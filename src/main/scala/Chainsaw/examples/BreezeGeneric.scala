package Chainsaw.examples

import breeze.math.Semiring
import breeze.linalg.{norm, _}
import breeze.math._
import breeze.numerics._
import breeze.signal._

import scala.reflect.{ClassTag, classTag}

case class ZInt(value: Int)

class Zp(p: Int) extends Semiring[ZInt] {
  override def zero = ZInt(0)

  override def one = ZInt(1)

  override def +(a: ZInt, b: ZInt) = ZInt((a.value + b.value) % p)

  override def *(a: ZInt, b: ZInt) = ZInt((a.value * b.value) % p)

  override def ==(a: ZInt, b: ZInt) = a.value == b.value

  override def !=(a: ZInt, b: ZInt) = a.value != b.value
}

object BreezeGeneric {

  def genericDft[T](data: DenseVector[T], omega: T, inverse: Boolean = false)
                   (implicit semiring: Semiring[T], classTag: ClassTag[T]) = {

    val N = data.length
    val factors = semiring.one +: (1 to N).map(i => product(DenseVector.fill(i)(omega)))
    println(factors.mkString(" "))
    require(factors.distinct.size == N && factors.last == factors.head)

    DenseVector.tabulate(N) { k =>
      val indices = (0 until N).map(i => if (inverse) (i * k) % N else -(i * k) % N + N)
      val coeffs = DenseVector.tabulate(N)(i => factors(indices(i)))
      sum(data *:* coeffs)
    }
  }

  def main(args: Array[String]): Unit = {
    import breeze.linalg._

    implicit val ring = new Zp(3329)
    val a = DenseVector(ZInt(3328))
    val b = DenseVector(ZInt(3328))
    println(a *:* b)

  }
}
