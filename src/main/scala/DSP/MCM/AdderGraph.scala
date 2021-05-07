package DSP.MCM

import DSP.MCM.ASSSign._
import DSP.MCM.ASSType._
import DSP.MCM.AdderGraph.AReverse
import DSP.RAGn.getPositiveOddFundamental
import org.jgrapht.alg.shortestpath.AllDirectedPaths
import org.jgrapht.graph._
import org.jgrapht.traverse._
import spinal.core._

import scala.collection.JavaConversions._

sealed trait ASSType
object ASSType {
  case object INPUT extends ASSType
  case object OUTPUT extends ASSType
  case object ASS extends ASSType
}

sealed trait ASSSign
object ASSSign {
  case object ADD extends ASSSign
  case object SUBNEXT extends ASSSign // prev - next
  case object SUBPREV extends ASSSign // next - prev
}

case class AConfigVector(shiftLeft0: Int, shiftLeft1: Int, shiftRight: Int, assSign: ASSSign)

case class AOperation(j: Int, k: Int, r: Int, assSign: ASSSign = ADD) {
  require((j > 0 && k == 0 && r == 0) || (j == 0 && k > 0 && r == 0) || (j == 0 && k == 0 && r > 0),
    "for positive odd operator, (j > 0 && k == 0) || (j == 0 && k > 0) || (j == k && j < 0)") //  for positive odd operator
}

object AOperation {
  def apply(j: Int, assSign: ASSSign): AOperation = AOperation(j, 0, 0, assSign)

  def apply(configVector: AConfigVector): AOperation =
    AOperation(configVector.shiftLeft0, configVector.shiftLeft1, configVector.shiftRight, configVector.assSign)
}

case class Shift(source: AddSubShift, des: AddSubShift, shiftLeft: Int = 0, negation: Boolean = false)

case class AddSubShift(assSign: ASSSign = ADD, shiftRight: Int = 0, value: Int, assType: ASSType = ASS)

/** This is an object-oriented model for shift-adder graph, based on
 *
 */
//  TODO: decouple the structure and the operator(shift-adder graph should be "binary graph" + "shift adder")
//  TODO: consider negation
//  TODO: consider the order of parallel edges, as left / right count by SUBNEXT/SUBPREV
class AdderGraph {
  val graph = new DirectedWeightedMultigraph[AddSubShift, Shift](classOf[Shift])
  graph.addVertex(AddSubShift(ADD, 0, 1, INPUT))

  def root = graph.vertexSet().find(_.value == 1).get

  def containsFundamental(that: Int) = graph.vertexSet().exists(_.value == that)

  def getFundamental(that: Int) = {
    require(containsFundamental(that), s"fundamental $that not found!")
    graph.vertexSet().find(_.value == that).get
  }

  def getFundamentalOption(that: Int) = {
    if (containsFundamental(that)) Some(graph.vertexSet().find(_.value == that).get)
    else None
  }

  def addFundamental(u: Int, v: Int, aOperation: AOperation): Boolean = {
    val as0 = getFundamental(u)
    val as1 = getFundamental(v)
    val value = aOperation.assSign match {
      case ASSSign.ADD => ((u << aOperation.j) + (v << aOperation.k)) >> aOperation.r
      case ASSSign.SUBNEXT => ((u << aOperation.j) - (v << aOperation.k)) >> aOperation.r
      case ASSSign.SUBPREV => ((v << aOperation.k) - (u << aOperation.j)) >> aOperation.r
    }
    val asNew = AddSubShift(aOperation.assSign, aOperation.r, value)
    graph.addVertex(asNew)
    graph.addEdge(as0, asNew, Shift(as0, asNew, aOperation.j))
    graph.addEdge(as1, asNew, Shift(as1, asNew, aOperation.k))
  }

  def addFundamental(u: Int, v: Int, w: Int): Boolean = {
    addFundamental(u, v, AOperation(AReverse(w, u, v)))
  }

  def addOutput(u: Int, shiftLeft: Int, negation: Boolean = false) = {
    val as = getFundamental(u)
    val value = (if (negation) -1 else 1) * (if (shiftLeft >= 0) u << shiftLeft else u >> (-shiftLeft))
    val asNew = AddSubShift(ADD, 0, value, OUTPUT)
    graph.addVertex(asNew)
    graph.addEdge(as, asNew, Shift(as, asNew, shiftLeft, negation))
  }

  def numberOfOutputs = graph.vertexSet().filter(_.assType == OUTPUT).size

  def outputs = graph.vertexSet().filter(_.assType == OUTPUT)

  def valueOfOutputs = graph.vertexSet().filter(_.assType == OUTPUT).map(_.value)

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
    adderGraph.addFundamental(1, 1, AOperation(2, SUBNEXT))
    adderGraph.addFundamental(1, 3, AOperation(4, ADD))
    adderGraph.addFundamental(3, 19, AOperation(3, ADD))
    //    adderGraph.addFundamental(3, 19, AOperation(2, SUBPREV))
    adderGraph.addFundamental(3, 19, 7)
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

  def AReverse(w: Int, u: Int, v: Int): AConfigVector = {
    require(w > 0 && u > 0 && v > 0 && w % 2 != 0 && u % 2 != 0 && v % 2 != 0, s"$u, $v, $w  fundamentals should be preprocessed into positive odd")
    println(s"$w,$u,$v")
    val cond1 = w == getPositiveOddFundamental(u + v)
    val cond2 = (u > v) && (w == getPositiveOddFundamental(u - v))
    val cond3 = (u < v) && (w == getPositiveOddFundamental(v - u))
    val cond4 = (w - u > 0) && ((w - u) % v == 0) && isPow2((w - u) / v) // w = 2 << i * v + u
    val cond5 = (w - u < 0) && (-(w - u) % v == 0) && isPow2((u - w) / v) // w = u - 2 << i * v
    val cond6 = isPow2(w + u) && ((w + u) % v == 0) // w = 2 << i * v - u
    val cond7 = (w - v > 0) && ((w - v) % u == 0) && isPow2((w - v) / u) // w = 2 << i * u + v
    val cond8 = (w - v < 0) && (-(w - v) % u == 0) && isPow2((v - w) / u) // w = v - 2 << i * u
    val cond9 = isPow2(w + v) && ((w + v) % u == 0) // w = 2 << i * u - v

    if (cond1) AConfigVector(0, 0, log2Up((u + v) / w), ADD)
    else if (cond2) AConfigVector(0, 0, log2Up((u + v) / w), SUBNEXT)
    else if (cond3) AConfigVector(0, 0, log2Up((u + v) / w), SUBPREV)
    else if (cond4) AConfigVector(0, log2Up((w - u) / v), 0, ADD)
    else if (cond5) AConfigVector(0, log2Up((u - w) / v), 0, SUBNEXT)
    else if (cond6) AConfigVector(0, log2Up((w + u) / v), 0, SUBPREV)
    else if (cond7) AConfigVector(log2Up((w - v) / u), 0, 0, ADD)
    else if (cond8) AConfigVector(log2Up((v - w) / u), 0, 0, SUBPREV)
    else if (cond9) AConfigVector(log2Up((w + v) / u), 0, 0, SUBNEXT)
    else AConfigVector(0, 0, 0, ADD)
  }
}