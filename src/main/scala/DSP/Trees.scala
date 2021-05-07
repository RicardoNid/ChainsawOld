package DSP

import spinal.core._

import scala.annotation.tailrec

/** Binary tree(bottom-top reversed, actually), connecting vertexex by binary operations
 *
 * @param input            input vector of signals, leaves of the tree
 * @param operator         the binary operator, the hardware building block of the tree
 * @param pipelineInterval distance between registers in terms of the depth of the tree, 0 as no pipelining
 * @tparam T type of signals in input vector
 * @see [[https://www.notion.so/SpinalHDL-Trees-9446624ca1594a41a29496cfd46f8605 Trees in Chainsaw]] for basic concepts about Chainsaw Trees
 */
class BinaryTree[T <: Data](input: Vec[T], operator: (T, T) => T, pipelineInterval: Int = 0) extends ImplicitArea[T] with Testable {

  private val width = input.length

  @tailrec
  private def buildTree(input: Vec[T], currentDepth: Int = 0): Vec[T] = {
    val width = input.length
    val doPipeline = pipelineInterval != 0 && currentDepth % pipelineInterval == pipelineInterval - 1
    width match {
      case 1 => input
      case _ =>
        val half = width / 2
        // extract the elements as left, right or remained operands
        val operandsLeft = (0 until half).map(i => input(i * 2))
        val operandsRight = (0 until half).map(i => input(i * 2 + 1))
        val remainder = if (width % 2 == 0) IndexedSeq[T]() else IndexedSeq(input.last)
        // operating
        val result = operandsLeft.zip(operandsRight).map { case (lt, rt) => operator(lt, rt) } ++ remainder
        // pipelining
        val pipelinedResult = if (doPipeline) result.map(RegNext(_)) else result
        // recursive call
        buildTree(Vec(pipelinedResult), currentDepth + 1)
    }
  }

  private val output = buildTree(input).head

  override def implicitValue: T = RegNext(output)

  private val latency = if (pipelineInterval == 0) 1 else log2Up(width) / pipelineInterval + 1
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, latency, 1)
}

// TODO: build connections between Binary Tree and Binary Tree with Info(at least, merge some part of them)
class BinaryTreeWithInfo[T <: Data, I](input: IndexedSeq[(T, I)], operator: ((T, I), (T, I)) => (T, I), pipelineInterval: Int = 0) extends ImplicitArea[T] with Testable {

  private val width = input.length

  @tailrec
  private def buildTree(input: IndexedSeq[(T, I)], currentDepth: Int = 0): Seq[(T, I)] = {
    val width = input.length
    val doPipeline = pipelineInterval != 0 && currentDepth % pipelineInterval == pipelineInterval - 1
    width match {
      case 1 => input
      case _ =>
        val half = width / 2
        // extract the elements as left, right or remained operands
        val operandsLeft = (0 until half).map(i => input(i * 2))
        val operandsRight = (0 until half).map(i => input(i * 2 + 1))
        val remainder = if (width % 2 == 0) IndexedSeq[(T, I)]() else IndexedSeq(input.last)
        // operating
        val result = operandsLeft.zip(operandsRight).map { case (lt, rt) => operator(lt, rt) } ++ remainder
        // pipelining
        val pipelinedResult = if (doPipeline) result.map { case (t, i) => (RegNext(t), i) } else result
        // recursive call
        buildTree(pipelinedResult, currentDepth + 1)
    }
  }

  private val output = buildTree(input).head

  override def implicitValue: T = RegNext(output._1)
  def getRemainedInfo = output._2

  private val latency = if (pipelineInterval == 0) 1 else log2Up(width) / pipelineInterval + 1
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, latency, 1)
}

object CommonTrees {

}