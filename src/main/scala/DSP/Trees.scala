package DSP

import spinal.core._

class BinaryTree[T <: Data](input: Vec[T], operator: (T, T) => T, pipelineInterval: Int = 0) extends ImplicitArea[T] with Testable {

  val width: Int = input.length

  def buildTree(input: Vec[T], currentDepth: Int = 0): Vec[T] = {
    val width = input.length
    val doPipeline = pipelineInterval != 0 && currentDepth % pipelineInterval == pipelineInterval - 1
    width match {
      case 1 => input
      case _ => {
        val half = width / 2
        // operating
        val operandsLeft = (0 until half).map(i => input(i * 2))
        val operandsRight = (0 until half).map(i => input(i * 2 + 1))
        val remainder = if (width % 2 == 0) IndexedSeq[T]() else IndexedSeq(input.last)
        val result = operandsLeft.zip(operandsRight).map { case (lt, rt) => operator(lt, rt) } ++ remainder
        // pipelining
        val pipelinedResult = if (doPipeline) result.map(RegNext(_))
        else result
        // recursive call
        buildTree(Vec(pipelinedResult), currentDepth + 1)
      }
    }
  }

  val output = buildTree(input).head

  override def implicitValue: T = RegNext(output)

  val latency = if (pipelineInterval == 0) 1 else log2Up(width) / pipelineInterval + 1
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, latency, 1)
}
