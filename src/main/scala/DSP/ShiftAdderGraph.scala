package DSP

import org.jgrapht.traverse.BreadthFirstIterator
import spinal.core._

import scala.collection.JavaConversions._
import scala.collection.mutable //  for digital signal processing

class ShiftAdderGraph(input: SFix, adderGraph: AdderGraph) extends ImplicitArea[SFix] with DSPDesign {

  val max = input.maxValue
  val min = input.minValue
  val resolution = input.resolution

  val binding = mutable.LinkedHashMap[AddSubShift, SFix]()
  binding += adderGraph.root -> input

  val bfsIter = new BreadthFirstIterator(adderGraph.graph)
  while (bfsIter.hasNext) {
    val vertex = bfsIter.next()
    if (vertex != adderGraph.root) {
      val edges = adderGraph.graph.incomingEdgesOf(vertex)
      val sources = edges.map(adderGraph.graph.getEdgeSource(_))
      val left = binding.get(sources.head).get << edges.head.shiftLeft
      val right = binding.get(sources.last).get << edges.last.shiftLeft
      val signal = left + right
      signal.setName(s"multiple_${vertex.value}")
      binding += vertex -> signal
    }
  }

  override def implicitValue: SFix = binding.last._2

  override def getDelay: Int = 0
}

object ShiftAdderGraph {
  def apply(input: SFix, adderGraph: AdderGraph): ShiftAdderGraph = new ShiftAdderGraph(input, adderGraph)
}

