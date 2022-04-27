package Chainsaw.dsl.dataflow

import Chainsaw._
import Chainsaw.dsl._
import spinal.core._

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

    val repetition0 = Repetition(Seq(
      SpaceRepetition(2, 1),
      SpaceRepetition(2)
    ), TimeRepetition(2))

    val reuse0 = Reuse(2, 2, 2, 4)

    val flow0 = PeriodicFlow((2, 2), repetition0, reuse0)
    flow0.drawInput
    flow0.drawOutput

    val repetition1 = Repetition(Seq(SpaceRepetition(2)), TimeRepetition(4))
    val reuse1 = Reuse(2, 4, 1, 1)
    val flow1 = PeriodicFlow((2, 3), repetition1, reuse1)
//    flow1.drawOutput

    val repetition2 = Repetition(Seq(SpaceRepetition(2)), TimeRepetition(5))
    val reuse2 = Reuse(2, 5, 1, 1)
    val flow2 = PeriodicFlow((2, 3), repetition2, reuse2)
//    flow2.drawOutput

    println(FlowConversion(flow1.outputFlow, flow0.inputFlow).toKaTex)
    println(FlowConversion(flow2.outputFlow, flow0.inputFlow).toKaTex)

    val conversion = FlowConversion(flow2.outputFlow, flow0.inputFlow)
    println(conversion.toKaTex)
    GenRTL(FlowConverter(conversion, new ForwardRegisterAllocator(conversion).getAllocation, Bits(8 bits)))

  }
}
