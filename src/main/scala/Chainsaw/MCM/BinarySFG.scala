package Chainsaw.MCM

import org.jgrapht.graph._

import scala.collection.JavaConversions._

class BinarySFG extends DirectedMultigraph[Int, DefaultEdge](classOf[DefaultEdge]) {

  def size = this.vertexSet().size()

  def apply(n: Int) = this.vertexSet().filter(_ == n).head

  def driversOf(v: Int) = this.incomingEdgesOf(v).toSeq.map(this.getEdgeSource(_))

  def inputs = this.vertexSet().filter(this.inDegreeOf(_) == 0)

  def outputs = this.vertexSet().filter(this.outDegreeOf(_) == 0)

  def addVertex(src0: Int, src1: Int): DefaultEdge = {
    val vertex = this.vertexSet().size()
    this.addVertex(vertex)
    this.addEdge(src0, vertex)
    this.addEdge(src1, vertex)
  }
}