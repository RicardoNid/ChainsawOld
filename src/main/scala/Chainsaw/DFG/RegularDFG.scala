package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

import org.jgrapht._
import org.jgrapht.graph._
import org.jgrapht.graph.builder._
import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import org.jgrapht.traverse._
import org.jgrapht.generate._

import scala.collection.JavaConversions._

object EdgeDirection extends Enumeration {
  type EdgeDirection = Value
  val LEFT, UPLEFT, UP, UPRIGHT, RIGHT, DOWNRIGHT, DOWN, DOWNLEFT = Value
}

import EdgeDirection._

case class RegularEdge(direction: EdgeDirection, outOrder: Int, inOrder: Int, delay: Double)

class RegularDFG[T](name:String) extends DFGGraph[T](name) {

  def build2D(row: Int, col: Int, node: DSPNode[T], edges: Seq[RegularEdge]) = {

    val nodes: Seq[Seq[DSPNode[T]]] = Seq.tabulate(row, col)((i, j) => node.copy(s"${node.name}_${i}_${j}"))
    nodes.flatten.foreach(addVertex)
    println(this)

    val directionMap = Map(LEFT -> (-1, 0), RIGHT -> (1, 0), UP -> (0, -1), DOWN -> (0, 1),
      UPLEFT -> (-1, -1), UPRIGHT -> (1, -1), DOWNLEFT -> (-1, 1), DOWNRIGHT -> (1, 1))

    edges.foreach { edge =>
      val indexDiff = directionMap(edge.direction)
      Seq.tabulate(row, col) { (i, j) =>
        nodes(i)(j)



      }
    }
  }
}

object RegularDFG {
  def main(args: Array[String]): Unit = {
    val dfg = new RegularDFG[Bits]("examplRegularGraph")
    val and: BinaryNode[Bits] = BinaryNode(Operators.and, "and")
    dfg.build2D(3, 3, and, Seq[RegularEdge]())
  }
}
