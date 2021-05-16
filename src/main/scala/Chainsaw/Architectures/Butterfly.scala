package Chainsaw.Architectures

import Chainsaw.{DSPDesign, Testable, TimingInfo}
import spinal.core._ //  for digital signal processing

class Butterfly[T <: Data](input: Vec[T]) extends ImplicitArea[Vec[T]] {

  require(isPow2(input.length), "Butterfly network requires a size as 2^n")
  val depth = log2Up(input.length)

  override def implicitValue = ???
}

class ButterflyBuilder[T](val input: Seq[T]) extends TreeBuilder[T] {

  require(isPow2(input.length))

  override val depth = log2Up(input.length)
  override val router = (input: Seq[T], currentDepth: Int) => {
    def swap(input: Seq[T]) = input.takeRight(input.length / 2) ++ input.take(input.length / 2)

    val groups: Seq[Seq[T]] = input.grouped(input.length / (1 << currentDepth)).toSeq
    groups.map(swap).flatten
  }
  override val operator: (Seq[T], Int) => Seq[T] = (input: Seq[T], currentDepth: Int) => input.map(term => term)
}

class AdderTree(val input: Seq[Int]) extends TreeBuilder[Int] {
  override val depth = log2Up(input.length)
  override val router = (input: Seq[Int], currentDepth: Int) => input.map(term => term)
  override val operator = (input: Seq[Int], currentDepth: Int) => {
    val temp = (0 until input.length / 2).map(i => input(2 * i) + input(2 * i + 1))
    if(input.length % 2 == 0) temp
    else temp :+ input.last
  }
}

object ButterflyBuilder {

  def main(args: Array[String]): Unit = {
    val builder0 = new ButterflyBuilder[Int]((0 until 16))
    println(builder0.implement().mkString(" "))
    val builder1 = new AdderTree((0 until 10))
    println(builder1.implement().mkString(" "))
  }

  import breeze.signal.fourierTr
  import breeze.linalg._

  println(fourierTr.dvDouble1DFFT(DenseVector(Array(1.0, 2.0, 3.0, 4.0))))


}