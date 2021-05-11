import org.jgrapht.alg.isomorphism.VF2GraphIsomorphismInspector
import org.jgrapht.graph._

val graph = new SimpleDirectedWeightedGraph[Int, DefaultEdge](classOf[DefaultEdge])
graph.addVertex(0)
graph.addVertex(1)
graph.addVertex(2)
graph.addVertex(3)
graph.addEdge(0, 1)
graph.addEdge(1, 3)
graph.addEdge(0, 2)
graph.addEdge(2, 3)
graph.setEdgeWeight(0, 1, 2)
graph.addEdge(1, 2)
val anotherGraph = new SimpleDirectedWeightedGraph[Int, DefaultEdge](classOf[DefaultEdge])
anotherGraph.addVertex(0)
anotherGraph.addVertex(1)
anotherGraph.addVertex(2)
anotherGraph.addVertex(3)
anotherGraph.addEdge(0, 1)
anotherGraph.addEdge(1, 3)
anotherGraph.addEdge(1, 2)
anotherGraph.addEdge(2, 3)
anotherGraph.addEdge(0, 1)
anotherGraph.addEdge(0, 2)
println(anotherGraph.getEdgeWeight(anotherGraph.getEdge(0, 1)))



val isoInspect = new VF2GraphIsomorphismInspector(graph, anotherGraph, false)
isoInspect.isomorphismExists()
println(anotherGraph)

