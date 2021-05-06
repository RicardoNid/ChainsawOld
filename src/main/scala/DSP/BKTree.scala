package DSP

import spinal.core._ //  for digital signal processing

class BKTree[T <: Data](input: Vec[T], op: (T, T) => T) extends ImplicitArea[Vec[T]] with DSPDesign with Testable {
  val width = input.length
  require(isPow2(width))
  val depth = log2Up(width) * 2 - 1

  def BuildTree(input: Vec[T], depthRemain: Int): Vec[T] = {

    println(s"depthReamin: $depthRemain")

    val halfDepth = (depth + 1) / 2
    val forward = depthRemain >= halfDepth

    val output = if (forward) {
      val step = 1 << (depth - depthRemain)
      val doubleStep = step << 1
      val operands = (0 until width).zip(input).filter(_._1 % step == 0).map(_._2)
      val sums = (0 until operands.length / 2).map(i => op(operands(2 * i), operands(2 * i + 1)))
      (0 until width).map { i =>
        if (i % doubleStep == 0) sums(i / doubleStep)
        else input(i)
      }
    }
    else {
      val doubleStep = width >> (halfDepth - depthRemain)
      val step = doubleStep >> 1
      val indexRight = (0 until width).filter(_ % doubleStep == 0).drop(1)
      val indexLeft = indexRight.map(_ - step)
      val sums = indexLeft.zip(indexRight).map { case (l, r) => op(input(l), input(r)) }
      (0 until width).map { i =>
        if ((i % doubleStep == step) && (i != (width - step))) sums(i / doubleStep)
        else input(i)
      }
    }

    depthRemain match {
      case 1 => Vec(output)
      case _ => BuildTree(Vec(output), depthRemain - 1)
    }
  }

  override def implicitValue: Vec[T] = RegNext(BuildTree(input, depth))

  override val start: Bool = Bool()
  override val busy: Bool = Bool()
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}


