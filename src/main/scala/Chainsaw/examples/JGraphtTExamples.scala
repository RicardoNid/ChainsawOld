package Chainsaw.examples

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._
import org.jgrapht.util.SupplierUtil

import scala.collection.JavaConverters._
import scala.collection.JavaConversions._
import java.io._
import java.net._
import java.util._
import java.util.function.Supplier

object JGraphtTExamples extends App {

  val g: DefaultDirectedGraph[URI, DefaultEdge] = new DefaultDirectedGraph[URI, DefaultEdge](classOf[DefaultEdge])
  val h = GraphTypeBuilder
    .directed[URI, DefaultEdge]()
    .allowingMultipleEdges(false)
    .allowingSelfLoops(true)
    .weighted(false)
    .edgeClass(classOf[DefaultEdge])
    .buildGraph()


  var google = new URI("http://www.google.com")
  val wikipedia = new URI("http://www.wikipedia.org")
  val jgrapht = new URI("http://www.jgrapht.org")

  // add the vertices
  g.addVertex(google)
  g.addVertex(wikipedia)
  g.addVertex(jgrapht)


  // add edges to create linking structure
  g.addEdge(jgrapht, wikipedia)
  g.addEdge(google, jgrapht)
  g.addEdge(google, wikipedia)
  g.addEdge(wikipedia, google)

  Graphs.addEdgeWithVertices(g, google,jgrapht)

  // find from sets
  println(g.vertexSet().filter(_.toString.contains("google")).mkString(" "))
  println(g.edgeSet().mkString(" "))

  // find from relationship
  // from a known edge
  println(g.getEdgeSource(g.getEdge(google, jgrapht)))
  println(g.getEdgeTarget(g.getEdge(google, jgrapht)))
  println(g.edgesOf(google).mkString(" "))
  // from a known vertex
  println(g.incomingEdgesOf(google).mkString(" "))
  println(g.outgoingEdgesOf(google).mkString(" "))
  // from two known vertex
  println(g.getEdge(google, jgrapht))
  println(g.getAllEdges(google, jgrapht).mkString(" "))

  println(new DepthFirstIterator(g, google).mkString(" "))

  val i = g.clone().asInstanceOf[DefaultDirectedGraph[URI, DefaultEdge]] // clone
  val chainsaw = new URI("http://www.chainsaw.com")
  i.addVertex(chainsaw)
  println(s"i: ${i.vertexSet().mkString(" ")}")
  println(s"g: ${g.vertexSet().mkString(" ")}")

  google = new URI("http://www.chainsaw.com")
  println(s"i: ${i.vertexSet().mkString(" ")}")
  println(s"g: ${g.vertexSet().mkString(" ")}")

}

// graph generated by generator
object CompleteGraphDemo { // number of vertices
  private val SIZE = 10

  /**
   * Main demo entry point.
   *
   * @param args command line arguments
   */
  def main(args: Array[String]): Unit = { // Create the VertexFactory so the generator can create vertices
    val vSupplier = new Supplier[String]() {
      private var id = 0

      override def get: String = "v" + {
        id += 1; id - 1
      }
    }
    // Create the graph object
    val completeGraph = new SimpleGraph[String, DefaultEdge](vSupplier, SupplierUtil.createDefaultEdgeSupplier, false)
    // Create the CompleteGraphGenerator object
    val completeGenerator = new CompleteGraphGenerator[String, DefaultEdge](SIZE)
    // Use the CompleteGraphGenerator object to make completeGraph a
    // complete graph with [size] number of vertices
    completeGenerator.generateGraph(completeGraph)
    // Print out the graph to be sure it's really complete
    val iter = new DepthFirstIterator[String, DefaultEdge](completeGraph)
    while ( {
      iter.hasNext
    }) {
      val vertex = iter.next
      System.out.println("Vertex " + vertex + " is connected to: " + completeGraph.edgesOf(vertex).toString)
    }
  }
}