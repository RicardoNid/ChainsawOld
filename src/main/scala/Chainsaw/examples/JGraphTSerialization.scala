package Chainsaw.examples

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import java.io._
import java.net._
import java.util._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import java.util.function._
import scala.compat.java8.FunctionConverters._

import scala.collection.JavaConversions._

object HelloJGraphT {
  def main(args: Array[String]): Unit = {
    val hrefGraph = new DefaultDirectedGraph[URI, DefaultEdge](classOf[DefaultEdge])

    val google = new URI("http://www.google.com");
    val wikipedia = new URI("http://www.wikipedia.org");
    val jgrapht = new URI("http://www.jgrapht.org");

    // add the vertices
    hrefGraph.addVertex(google)
    hrefGraph.addVertex(wikipedia)
    hrefGraph.addVertex(jgrapht)

    // add edges to create linking structure
    hrefGraph.addEdge(jgrapht, wikipedia)
    hrefGraph.addEdge(google, jgrapht)
    hrefGraph.addEdge(google, wikipedia)
    hrefGraph.addEdge(wikipedia, google)

    val start = hrefGraph.vertexSet().toSeq.filter(uri => uri.getHost().equals("www.jgrapht.org")).head

    import org.jgrapht.graph.DefaultEdge
    import java.io.StringWriter
    import java.io.Writer

    val exporter = new DOTExporter[URI, DefaultEdge]()
    val vertexIdProvider = (uri:URI) => uri.getHost.replace('.', '_')
    exporter.setVertexIdProvider(vertexIdProvider.asJava)
    val writer = new StringWriter()
    exporter.exportGraph(hrefGraph, writer)
    println(writer.toString)


  }
}