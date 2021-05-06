package DSP

import spinal.core._ //  for digital signal processing

/** This is not a module, not even a module generator - this is an architecture.
 *
 * It implements prefix sum by a hybrid architecture of Brent-Kung and Kogge-Stone trees
 *
 * @param input    input signals
 * @param operator the associative binary operator, which is a hardware building block
 * @param BKLevel  level of Brent-Kung tree, you can trade 2 level of BK for 1 level of KS
 * @tparam T type of signals in input vector
 * @see [[https://www.notion.so/SpinalHDL-Trees-9446624ca1594a41a29496cfd46f8605 Trees in Chainsaw]]
 */
class BKTree[T <: Data](input: Vec[T], operator: (T, T) => T, BKLevel: Int = 0) extends ImplicitArea[Vec[T]] with DSPDesign with Testable {
  val width: Int = input.length
  require(isPow2(width))
  val depth: Int = log2Up(width) * 2

  def BuildTree(input: Vec[T], currentDepth: Int): Vec[T] = {

    require(BKLevel <= depth / 2)

    val isBKForward = currentDepth < BKLevel
    val isKS = !isBKForward && currentDepth < (depth - BKLevel)
    val isMid = currentDepth == depth / 2 - 1

    println(s"current $currentDepth, isKS: $isKS")

    def BKForward(step: Int) = {
      val doubleStep = step << 1
      val operandsLeft = input.zipWithIndex.filter(_._2 % doubleStep == 0)
      val operandsRight = operandsLeft.map { case (_, i) => (input(i + step), i + step) }
      (operandsLeft, operandsRight)
    }

    def BKBackword(step: Int) = {
      val doubleStep = step << 1
      val operandsRight = input.zipWithIndex.filter(_._2 % doubleStep == 0).drop(1)
      val operandsLeft = operandsRight.map { case (_, i) => (input(i - step), i - step) }
      (operandsLeft, operandsRight)
    }

    def KoggeStone(level: Int) = { // start starts from 0, level starts from 0
      val candidateStep = 1 << BKLevel
      val candidates = input.zipWithIndex.filter(_._2 % candidateStep == 0)
      val operatorNum = candidates.length - (1 << level)
      val operandsLeft = candidates.take(operatorNum)
      val operandsRight = candidates.slice(1 << level, operatorNum + 1 << level)
      (operandsLeft, operandsRight)
    }

    val (operandsLeft, operandsRight) = {
      if (isBKForward) BKForward(1 << currentDepth)
      else if (isKS) KoggeStone((currentDepth - BKLevel) / 2)
      else BKBackword(1 << (depth - currentDepth - 1))
    }
    val updates = operandsLeft.zip(operandsRight).map { case ((tl, il), (tr, _)) => (operator(tl, tr), il) }

    val updateIndexes = updates.map(_._2)

    val output = (0 until width).map { i =>
      if (updateIndexes.contains(i)) updates.find(_._2 == i).get._1
      else input(i)
    }

    val increment = if (isMid || isKS) 2 else 1
    if (currentDepth + increment > depth - 1) Vec(output)
    else BuildTree(Vec(output), currentDepth + increment)
  }

  override def implicitValue: Vec[T] = RegNext(BuildTree(input, 0))

  override val start: Bool = Bool()
  override val busy: Bool = Bool()
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 1, 1)
}

