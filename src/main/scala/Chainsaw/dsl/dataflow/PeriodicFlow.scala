package Chainsaw.dsl.dataflow

import Chainsaw.dsl._

case class PeriodicFlow(size: (Int, Int), repetition: Repetition, reuse: Reuse) {

  def getBubble(data: Array[Array[Int]]) = data.map(_.map(_ => -1))

  type Slice = Array[Int]

  val inPortWidth = size._1 * repetition.spaceFactor / reuse.spaceReuse / reuse.fold
  val outPortWidth = size._2 * repetition.spaceFactor / reuse.spaceReuse / reuse.fold

  val inputSize = repetition.expand(size)._1
  val outputSize = repetition.expand(size)._2

  val inputRange = (0 until inputSize).toArray
  val outputRange = (0 until outputSize).toArray
  val inputSegments: Array[Slice] = repetition.divide(inputRange)
  val outputSegments: Array[Slice] = outputRange.divide(repetition.spaceFactor)

  def segments2Iteration(segments: Array[Slice], portWidth: Int) = {
    segments
      .divide(reuse.spaceReuse)
      .map { slices =>
        val subSlices: Array[Array[Slice]] = slices.map(_.divide(reuse.fold))
        val reordered = (0 until reuse.fold).map(i => subSlices.map(_.apply(i)).flatten).toArray
        reordered
      }.flatten.padTo(reuse.iterationLatency, Array.fill(portWidth)(-1))
  }

  def iteration2Sequence(iteration: Array[Slice]) = {
    (iteration +: Array.fill(reuse.timeReuse - 1)(getBubble(iteration))).reduce(_ ++ _)
  }

  val inputSequence = iteration2Sequence(segments2Iteration(inputSegments, inPortWidth))
  val outputSequence = iteration2Sequence(segments2Iteration(outputSegments, outPortWidth))

  val inputFlow = BasicFlowAnalyser(inputSequence.map(_.toSeq).toSeq)
  val outputFlow = BasicFlowAnalyser(outputSequence.map(_.toSeq).toSeq)

  def drawInput = inputFlow.generateJsonFile("input", "x")
  def drawOutput = outputFlow.generateJsonFile("output", "y")

}

object PeriodicFlow {
  def main(args: Array[String]): Unit = {
    val repetition = Repetition(Seq(
      SpaceRepetition(2, 1),
      SpaceRepetition(2)
    ), TimeRepetition(2))

    val reuse = Reuse(2, 2, 2, 6)

    val flow = PeriodicFlow((2, 2), repetition, reuse)
    flow.drawInput
    flow.drawOutput
  }
}
