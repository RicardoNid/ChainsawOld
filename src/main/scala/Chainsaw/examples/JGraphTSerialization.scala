package Chainsaw.examples

import org.jgrapht.graph._
import org.jgrapht.nio.dot._

import java.net._
import scala.collection.JavaConversions._
import scala.compat.java8.FunctionConverters._

/** this example is from [[]] originally written in Java to convert java <-> scala functions
  * @see
  *   [[]]
  */
object HelloJGraphT {
  def main(args: Array[String]): Unit = {
    val hrefGraph = new DefaultDirectedGraph[URI, DefaultEdge](classOf[DefaultEdge])

    val google    = new URI("http://www.google.com");
    val wikipedia = new URI("http://www.wikipedia.org");
    val jgrapht   = new URI("http://www.jgrapht.org");

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

    val exporter         = new DOTExporter[URI, DefaultEdge]()
    val vertexIdProvider = (uri: URI) => uri.getHost.replace('.', '_')
    // to convert scala function -> java function
    exporter.setVertexIdProvider(vertexIdProvider.asJava)
    val writer = new StringWriter()
    exporter.exportGraph(hrefGraph, writer)
    println(writer.toString)
  }
}
