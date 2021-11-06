package Chainsaw.DFG

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.{logger, _}
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
  val LEFT, UPLEFT, UP, UPRIGHT, RIGHT, DOWNRIGHT, DOWN, DOWNLEFT = Value    // Edge Placement Direction
  val IN, OUT = Value    // Edge InOut Direction
}

import EdgeDirection._

case class RegularEdge(direction: EdgeDirection, outOrder: Int, inOrder: Int, delay: Double)
//case class RegularEdge(edgeDirection: Value, InOut: Value, outOrder: Int, inOrder: Int, delay: Int, withInput: Boolean, withOutput: Boolean)

class RegularDFG[T](name:String) extends DFGGraph[T](name) {

  // TODO: 2D Irregular shape Systolic Array (not rectangle)
  // 2D Regular Systolic Array
  def build2D(row: Int, col: Int, node: DSPNode[T], edges: Seq[RegularEdge]) = {

    // define all nodes in the 2D Systolic Array

    // Systolic cells that perform calculations
    val nodes: Seq[Seq[DSPNode[T]]] = Seq.tabulate(row, col)((i, j) => node.copy(s"${node.name}_${i}_${j}"))
    println("Systolic Array:")
    println(nodes.map(_.mkString(" ")).mkString("\n"))
    nodes.flatten.foreach(addVertex)

//    val directionMap = Map(
//      LEFT -> (-1, 0), RIGHT -> (1, 0), UP -> (0, -1), DOWN -> (0, 1),
//      UPLEFT -> (-1, -1), UPRIGHT -> (1, -1), DOWNLEFT -> (-1, 1), DOWNRIGHT -> (1, 1))
//
//    edges.foreach { edge =>
//      val indexDiff = directionMap(edge.direction)
//      Seq.tabulate(row, col) { (i, j) =>
//        nodes(i)(j)
//
//
//      }
//    }

    val directionMap = Map(LEFT -> (-1, 0), RIGHT -> (1, 0), UP -> (0, -1), DOWN -> (0, 1),
      UPLEFT -> (-1, -1), UPRIGHT -> (1, -1), DOWNLEFT -> (-1, 1), DOWNRIGHT -> (1, 1))


    /** Chain all Edges in that given direction
     * {{{In Out Ports of a cell is symmetrical: for each input port there will be an output port}}}
     * @param edgeDirection specify one edgeDirection from eight directions
     * @param InOut from the view of each cell, should each edge of such edgeDirection coming In or Out?
     * @param outOrder from the view of each cell, ...
     * @param inOrder from the view of each cell, ...
     * @param delay reg between cells
     * @param withInput whether that direction of chain has Input
     * @param withOutput whether that direction of chain has Output
     */
    def addAllEdgeChain(edgeDirection: Value, InOut: Value, outOrder: Int, inOrder: Int, delay: Int, withInput: Boolean, withOutput: Boolean) = {

      // edgeDirection -> (delta_row, delta_col)
      val directionMap = Map(
        UPLEFT   -> (-1, -1),  UP -> (-1, 0),  UPRIGHT -> (-1, 1),
        LEFT     -> (0, -1),                     RIGHT -> (0, 1),
        DOWNLEFT -> (1, -1), DOWN -> (1, 0), DOWNRIGHT -> (1, 1)
      )

      val indexDiff = directionMap(edgeDirection)
      println(s"$edgeDirection, ${indexDiff._1}, ${indexDiff._2}, $InOut, $withInput, $withOutput\n")    // testing print

      val cellRowRange = (0 until row).toList    // range: [0, 1, ..., row-1]
      val cellColRange = (0 until col).toList    // range: [0, 1, ..., col-1]
//      println(s"row: $cellRowRange")
//      println(s"col: $cellColRange")

      for (r <- -1 to row) {        // wider range: [-1, 0, 1, ..., row]
        for (c <- -1 to col) {      // wider range: [-1, 0, 1, ..., col]
//          println(s"here ($r, $c)")
          var currInRange: Boolean = cellRowRange.contains(r) && cellColRange.contains(c)                                  // current node is in range
          var nextInRange: Boolean = cellRowRange.contains(r + indexDiff._1) && cellColRange.contains(c + indexDiff._2)    // next node is in range

          if (currInRange && nextInRange) {
            // current node in range and next node in range, use both current node and next node to add inner edges
            InOut match {
              case IN => this.addEdge(nodes(r + indexDiff._1)(c + indexDiff._2), nodes(r)(c), outOrder, inOrder, 1)  // next node -> curr node
              case OUT => this.addEdge(nodes(r)(c), nodes(r + indexDiff._1)(c + indexDiff._2), outOrder, inOrder, 1)  // curr node -> next node
              case _ => logger.error("Wrong InOut type")
            }
          } else if (!currInRange && nextInRange) {
            // current node not in range but next node in range, use next node to do withInput or do withOutput
            InOut match {
              case IN => if (withOutput) { println(s"next (${r+indexDiff._1}, ${c+indexDiff._2}) for Output")
                this.setOutput(nodes(r+indexDiff._1)(c+indexDiff._2), outOrder, s"output_${edgeDirection}_${r+indexDiff._1}_${c+indexDiff._2}", Seq(Schedule(0, 1)))}
              case OUT => if (withInput) { println(s"next (${r+indexDiff._1}, ${c+indexDiff._2}) for Input")
                this.setInput(nodes(r+indexDiff._1)(c+indexDiff._2), inOrder, s"input_${edgeDirection}_${r+indexDiff._1}_${c+indexDiff._2}", Seq(Schedule(0, 1)))}
              case _ => logger.error("Wrong InOut type")
            }
          } else if (currInRange && !nextInRange) {
            // current node in range but next node not in range, use current node to do withInput or do withOutput
            InOut match {
              case IN => if (withInput) { println(s"curr ($r, $c) for Input")
                this.setInput(nodes(r)(c), inOrder, s"input_${edgeDirection}_${r}_${c}", Seq(Schedule(0, 1)))}
              case OUT => if (withOutput) { println(s"curr ($r, $c) for Output")
                this.setOutput(nodes(r)(c), outOrder, s"output_${edgeDirection}_${r}_${c}", Seq(Schedule(0, 1)))}
              case _ => logger.error("Wrong InOut type")
            }
          }
        }  // for col
      }  // for row
    } // def addAllEdgeChain

    // testing
    // TODO: Clean this
//    val addingAllEdgeChain_UPLEFT0 = addAllEdgeChain(UPLEFT, OUT,0,0,1,true,false)
    val addingAllEdgeChain_UPLEFT0 = addAllEdgeChain(UPRIGHT, IN,0,0,1,true,true)
//    val addingAllEdgeChain_UP0 = addAllEdgeChain(UP, IN,1,1,1,true,false)
//    val addingAllEdgeChain_UP1 = addAllEdgeChain(UP, IN,2,2,1,true,true)

    println()
    println(this)  // print all Nodes and Edges

  }
}

object RegularDFG {
  def main(args: Array[String]): Unit = {
    val dfg = new RegularDFG[Bits]("examplRegularGraph")
    val and: BinaryNode[Bits] = BinaryNode(Operators.and, "and")
    dfg.build2D(3, 4, and, Seq[RegularEdge]())
  }
}
