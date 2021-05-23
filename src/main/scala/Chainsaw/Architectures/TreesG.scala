package Chainsaw.Architectures

import org.jgrapht.graph._
import org.jgrapht.nio.dot.{DOTExporter, DOTImporter}

import java.io.{StringReader, StringWriter}
import scala.collection.JavaConversions._

case class TreeVertex(depth: Int, id: Int)

abstract class TreesG extends DirectedMultigraph[TreeVertex, DefaultEdge](classOf[DefaultEdge]) {

  def addInputs(N: Int) = (0 until N).foreach(i => this.addVertex(TreeVertex(0, i)))

  def outputs = this.vertexSet().filter(this.outDegreeOf(_) == 0)

  def addLayer(): Unit
}
