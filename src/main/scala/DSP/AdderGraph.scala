package DSP

import DSP.ASSType._
import org.jgrapht.alg.shortestpath.AllDirectedPaths
import org.jgrapht.graph._
import org.jgrapht.traverse._

import scala.collection.JavaConversions._

sealed trait ASSType

object ASSType {

  case object INPUT extends ASSType

  case object OUTPUT extends ASSType

  case object ASS extends ASSType

}

case class AOperation(j: Int = 0, k: Int = 0, r: Int = 0, add: Boolean = true) {
  require((j > 0 && k == 0 && r == 0) || (j == 0 && k > 0 && r == 0) || (j == 0 && k == 0 && r > 0),
    "for positive odd operator, (j > 0 && k == 0) || (j == 0 && k > 0) || (j == k && j < 0)") //  for positive odd operator
  override def toString: String = s"AOperation: ${s"(x << $j) ${if (add) "+" else "-"} (y << $k)"}"
}

case class Shift(source: AddSubShift, des: AddSubShift, shiftLeft: Int = 0)

case class AddSubShift(add: Boolean = true, shiftRight: Int = 0, value: Int, assType: ASSType = ASS)

class AdderGraph {
  val graph = new DirectedWeightedMultigraph[AddSubShift, Shift](classOf[Shift])
  graph.addVertex(AddSubShift(true, 0, 1, INPUT))

  def containsFundamental(that: Int) = graph.vertexSet().exists(_.value == that)

  def getFundamentalOption(that: Int) = {
    if (containsFundamental(that)) Some(graph.vertexSet().find(_.value == that).get)
    else None
  }

  def getFundamental(that: Int) = {
    require(containsFundamental(that), s"fundamental $that not found!")
    graph.vertexSet().find(_.value == that).get
  }

  def addFundamental(u: Int, v: Int, aOperation: AOperation) = {
    val as0 = getFundamental(u)
    val as1 = getFundamental(v)
    val value = if (aOperation.add) ((u << aOperation.j) + (v << aOperation.k)) >> aOperation.r else ((u << aOperation.j) - (v << aOperation.k)) >> aOperation.r
    //    println(s"new value $value")
    val asNew = AddSubShift(aOperation.add, aOperation.r, value)
    graph.addVertex(asNew)
    graph.addEdge(as0, asNew, Shift(as0, asNew, aOperation.j))
    graph.addEdge(as1, asNew, Shift(as1, asNew, aOperation.k))
  }

  def addOutput(u: Int, shiftLeft: Int) = {
    val as = getFundamental(u)
    val value = if (shiftLeft >= 0) u << shiftLeft else u >> (-shiftLeft)
    val asNew = AddSubShift(true, 0, value, OUTPUT)
    graph.addVertex(asNew)
    graph.addEdge(as, asNew, Shift(as, asNew, shiftLeft))
  }

  def criticalPath(u: Int, v: Int) = {
    //  TODO: pick a suitable algo
    val allPathsAlg = new AllDirectedPaths(graph)
    val paths = allPathsAlg.getAllPaths(getFundamental(u), getFundamental(v), true, graph.vertexSet().size())
    paths.map(_.getLength).max
  }
}

object AdderGraph {
  def main(args: Array[String]): Unit = {
    val adderGraph = new AdderGraph
    adderGraph.addFundamental(1, 1, AOperation(2, 0, 0, false))
    adderGraph.addFundamental(1, 3, AOperation(4, 0, 0))
    adderGraph.addFundamental(3, 19, AOperation(3, 0, 0))
    adderGraph.addOutput(19, 4)

    println(adderGraph.graph.vertexSet().mkString("\n"))
    println(adderGraph.graph.edgeSet().mkString("\n"))
    println(adderGraph.graph.inDegreeOf(adderGraph.getFundamental(43)))

    val dfsIter = new DepthFirstIterator(adderGraph.graph)
    val bfsIter = new BreadthFirstIterator(adderGraph.graph)
    println(s"DFS: ${dfsIter.toSeq.map(_.value).mkString("->")}")
    println(s"BFS: ${bfsIter.toSeq.map(_.value).mkString("->")}")

    //    val scAlg = new KosarajuStrongConnectivityInspector(adderGraph.graph)
    //    println(s"strongly connected: ${scAlg.stronglyConnectedSets().size()}")

    println(s"critical path 1 -> 19: ${adderGraph.criticalPath(1, 19)}")


  }
}