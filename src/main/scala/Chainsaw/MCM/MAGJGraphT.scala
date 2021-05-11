package Chainsaw.MCM

import org.jgrapht.alg.cycle.CycleDetector
import org.jgrapht.alg.isomorphism.VF2GraphIsomorphismInspector
import org.jgrapht.graph.{DefaultEdge, _}

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

object MAGJGraphT {
  type MAG = SimpleDirectedWeightedGraph[Int, DefaultEdge]

  def main(args: Array[String]): Unit = {
    val graphs = generateMAG(3)
    println(graphs.length)
    println(graphs.forall(isValid))
    println(graphs.mkString("\n"))
    println(graphs.map(graph => graph.vertexSet().toSeq.map(sumOfIncomingEdges(graph, _)).mkString(" ")).mkString("\n"))
    println()
    println(graphs.map(graph => graph.edgeSet().toSeq.map(graph.getEdgeWeight(_)).mkString(" ")).mkString("\n"))
  }

  def distinctMAGs(graphs: Seq[MAG]) = {
    def eliminate(graphs: Seq[MAG]): Seq[MAG] = {
      println(s"distinct: ${graphs.length}")
      if (graphs.length == 1) graphs
      else {
        val notIso = graphs.drop(1).filterNot(graph =>
          new VF2GraphIsomorphismInspector(graphs(0), graph).isomorphismExists())
        graphs(0) +: eliminate(notIso)
      }
    }

    eliminate(graphs)
  }

  def noCycle(graph: MAG) = !new CycleDetector(graph).detectCycles()

  def noCycleMAGs(graphs: Seq[MAG]) = graphs.filter(noCycle)

  def isValid(graph: MAG): Boolean = graph.vertexSet().filterNot(_ == 0).map(v => sumOfIncomingEdges(graph, v)).forall(_ == 2) && // inDegrees == 2
    noCycle(graph)

  def copy(graph: MAG) = graph.clone().asInstanceOf[MAG]

  def sumOfIncomingEdges(graphs: MAG, v: Int) =
    graphs.incomingEdgesOf(v).toSeq.map(graphs.getEdgeWeight(_)).sum

  def generateMAG(cost: Int) = {

    // define src and des
    val graph = new SimpleDirectedWeightedGraph[Int, DefaultEdge](classOf[DefaultEdge])
    val connectedToSrc = ListBuffer[Int]()
    val connectedToDes = ListBuffer[Int]()
    connectedToSrc += 0
    connectedToDes += cost
    val des = cost

    def addSrc(graphs: Seq[MAG], v: Int) = {
      val srcAdded = graphs.map(graph =>
        connectedToSrc.filter(_ < v).map { src =>
          val newGraph = copy(graph)
          if (newGraph.containsEdge(src, v)) newGraph.setEdgeWeight(src, v, 2)
          else newGraph.addEdge(src, v)
          newGraph
        }
      ).flatten
      connectedToSrc += v
      srcAdded
    }

    def addDes(graphs: Seq[MAG], v: Int) = {
      val bothAdded = graphs.map(graph =>
        connectedToDes.filter(_ > v).filter(sumOfIncomingEdges(graph, _) < 2)
          .map { des =>
            val newGraph = copy(graph)
            if (newGraph.containsEdge(v, des)) newGraph.setEdgeWeight(v, des, 2)
            else newGraph.addEdge(v, des)
            newGraph
          }
          .filter(noCycle)
      ).flatten
      connectedToDes += v
      bothAdded
    }

    def addIncoming(graphs: Seq[MAG], v: Int) = {

      val anotherSrcAdded = graphs.map { graph =>
        if (sumOfIncomingEdges(graph, v) == 2) Seq(graph)
        else {
          val candidates = graph.vertexSet().toSeq.filter(_ < v).filterNot(graph.outgoingEdgesOf(v).contains(_))
          candidates
            .map { src =>
              val newGraph = copy(graph)
              if (newGraph.containsEdge(src, v)) newGraph.setEdgeWeight(src, v, 2)
              else newGraph.addEdge(src, v)
              newGraph
            }
            .filter(noCycle)
        }
      }.flatten
      anotherSrcAdded
    }

    (0 until cost + 1).foreach(graph.addVertex(_))

    val ret = if (cost + 1 - 2 == 0) {
      graph.addEdge(0, cost)
      graph.setEdgeWeight(0, cost, 2)
      Seq(graph)
    }
    else {
      var graphs = Seq(graph)
      (1 until cost + 1).foreach(v => graphs = addSrc(graphs, v))
      graphs = distinctMAGs(graphs)

      println(graphs.mkString("\n"))
      println("sum of weights")
      println(graphs.map(graph => graph.vertexSet().toSeq.map(sumOfIncomingEdges(graph, _)).mkString(" ")).mkString("\n"))
      println("weights")
      println(graphs.map(graph => graph.edgeSet().toSeq.map(graph.getEdgeWeight(_)).mkString(" ")).mkString("\n"))


      (1 until cost).foreach(v => graphs = addDes(graphs, v))
      graphs = distinctMAGs(graphs)
      (1 until cost + 1).foreach(v => graphs = addIncoming(graphs, v))
      graphs = distinctMAGs(graphs)
      graphs
    }
    ret
  }
}
