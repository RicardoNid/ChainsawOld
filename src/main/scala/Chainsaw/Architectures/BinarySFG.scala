package Chainsaw.Architectures

import Chainsaw.MCM.intSupplier
import org.jgrapht.graph._
import org.jgrapht.nio.dot.{DOTExporter, DOTImporter}

import java.io.{StringReader, StringWriter}
import scala.collection.JavaConversions._

class BinarySFG extends DirectedMultigraph[Int, DefaultEdge](classOf[DefaultEdge]) {

  def copy = this.clone().asInstanceOf[BinarySFG]

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

  def serialized = {
    val exporter = new DOTExporter[Int, DefaultEdge]()
    val writer = new StringWriter()
    exporter.exportGraph(this, writer)
    writer.toString.filterNot(_ == '\n')
  }
}

object BinarySFG {

  def fromSerialized(serialized: String): BinarySFG = {
    val ret = new BinarySFG()
    val importer = new DOTImporter[Int, DefaultEdge]()
    val reader = new StringReader(serialized)
    ret.setVertexSupplier(intSupplier)
    importer.importGraph(ret, reader)
    intSupplier.clear()
    ret
  }

  def main(args: Array[String]): Unit = {
    val graph = new BinarySFG()
    graph.addVertex(0)
    graph.addVertex(0, 0)
    val s = graph.serialized
    println(fromSerialized(s))
  }
}