package DSP

import DSP.ASSSign._
import DSP.ASSType._
import org.jgrapht.traverse.BreadthFirstIterator
import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable

class ShiftAdderGraph(input: SFix, adderGraph: AdderGraph) extends ImplicitArea[IndexedSeq[SFix]] with DSPDesignOld {

  val max = input.maxValue.toDouble
  val min = input.minValue.toDouble
  val resolution = input.resolution.toDouble

  val binding = mutable.LinkedHashMap[AddSubShift, SFix]()
  binding += adderGraph.root -> input

  val bfsIter = new BreadthFirstIterator(adderGraph.graph)
  while (bfsIter.hasNext) {
    val vertex = bfsIter.next()
    if (vertex != adderGraph.root) {
      val edges = adderGraph.graph.incomingEdgesOf(vertex)
      val sources = edges.map(adderGraph.graph.getEdgeSource(_))
      val coefficient = vertex.value
      val upper = if (coefficient < 0) coefficient * min else coefficient * max
      val lower = if (coefficient < 0) coefficient * max else coefficient * min
      val signal = MySFix(upper, lower, resolution * coefficient)
      if (vertex.assType == ASS) {
        val left = binding.get(adderGraph.graph.getEdgeSource(edges.head)).get << edges.head.shiftLeft
        val right = binding.get(adderGraph.graph.getEdgeSource(edges.last)).get << edges.last.shiftLeft
        if (vertex.assSign == ADD) signal := ((left +^ right) >> vertex.shiftRight).truncated
        else if (vertex.assSign == SUBNEXT) signal := ((left -^ right) >> vertex.shiftRight).truncated
        else signal := ((right -^ left) >> vertex.shiftRight).truncated
        signal.setName(s"multiple_${if (vertex.value < 0) "minus_" else ""}${if (vertex.value < 0) -vertex.value else vertex.value}")
        binding += vertex -> signal
      }
      if (vertex.assType == OUTPUT) {
        if (!edges.head.negation) signal := (binding.get(sources.head).get << edges.head.shiftLeft).truncated
        else signal := (-(binding.get(sources.head).get << edges.head.shiftLeft)).truncated
        signal.setName(s"output_${if (coefficient < 0) -coefficient else coefficient}")
        binding += vertex -> signal
      }
    }
  }

  override def implicitValue: IndexedSeq[SFix] = binding.filter(_._1.assType == OUTPUT).toIndexedSeq.sortBy(_._1.value).map(_._2)

  override def getDelay: Int = 0
}

object ShiftAdderGraph {
  def apply(input: SFix, adderGraph: AdderGraph): ShiftAdderGraph = new ShiftAdderGraph(input, adderGraph)
}

