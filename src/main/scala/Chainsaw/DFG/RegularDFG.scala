package Chainsaw.DFG

import org.jgrapht.nio._
import org.jgrapht.nio.dot._
import spinal.core._
import breeze.linalg._
import breeze.numerics._


object EdgeDirection extends Enumeration {
  type EdgeDirection = Value
  val LEFT, UPLEFT, UP, UPRIGHT, RIGHT, DOWNRIGHT, DOWN, DOWNLEFT = Value    // Edge Placement Direction
  val IN, OUT = Value    // Edge InOut Direction
}

import Chainsaw.DFG.EdgeDirection._

case class RegularEdge(edgeDirection: Value, InOut: Value, outOrder: Int, inOrder: Int, delay: Int, withInput: Boolean, withOutput: Boolean)

class RegularDFG[T <: Data](name: String) extends DFGGraph[T](name) {

  // TODO: 2D Irregular shape Systolic Array (not rectangle)
  // 2D Regular Systolic Array
  def build2D(row: Int, col: Int, node: DSPNode[T], edges: Seq[RegularEdge]): Unit = {

    // define all nodes in the 2D Systolic Array

    // Systolic cells that perform calculations
    val nodes: Seq[Seq[DSPNode[T]]] = Seq.tabulate(row, col) { (i, j) =>
      val newName = s"${node.name}_${i}_${j}"
      //println(newName)
      node.copy(newName)
    }
    println("Systolic Array:")
    println(nodes.map(_.mkString(" ")).mkString("\n"))
    nodes.flatten.foreach(addVertex)

    // used for graphviz displaying
    var inoutNodeAttrSeq = collection.mutable.Map[String, List[Int]]()


    /** Chain all Edges in that given direction
     * {{{In Out Ports of a cell is symmetrical: for each input port there will be an output port}}}
     *
     * @param edgeDirection specify one edgeDirection from eight directions
     * @param InOut         from the view of each cell, should each edge of such edgeDirection coming In or Out?
     * @param outOrder      from the view of each cell, ...
     * @param inOrder       from the view of each cell, ...
     * @param delay         reg between cells
     * @param withInput     whether that direction of chain has Input
     * @param withOutput    whether that direction of chain has Output
     */
    def addAllEdgeChain(edgeDirection: Value, InOut: Value, outOrder: Int, inOrder: Int, delay: Int, withInput: Boolean, withOutput: Boolean): Unit = {

      // edgeDirection -> (delta_row, delta_col)
      val directionMap = Map(
        UPLEFT   -> (-1, -1),  UP -> (-1, 0),  UPRIGHT -> (-1, 1),
        LEFT     -> (0, -1),                     RIGHT -> (0, 1),
        DOWNLEFT -> (1, -1), DOWN -> (1, 0), DOWNRIGHT -> (1, 1)
      )

      val indexDiff = directionMap(edgeDirection)
      println(s"edgeDirection = $edgeDirection, (${indexDiff._1}, ${indexDiff._2}), InOut = $InOut, withInput = $withInput, withOutput = $withOutput")    // testing print

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
              case IN => if (withOutput) {
                //println(s"next (${r+indexDiff._1}, ${c+indexDiff._2}) for Output")
                this.setOutput(nodes(r+indexDiff._1)(c+indexDiff._2), outOrder, s"output_${edgeDirection}_${r+indexDiff._1}_${c+indexDiff._2}($outOrder)", Seq(Schedule(0, 1)))
                inoutNodeAttrSeq += (s"output_${edgeDirection}_${r+indexDiff._1}_${c+indexDiff._2}($outOrder)" -> List(r+indexDiff._1, c+indexDiff._2, outOrder, indexDiff._1, indexDiff._2))
              }
              case OUT => if (withInput) {
                //println(s"next (${r+indexDiff._1}, ${c+indexDiff._2}) for Input")
                this.setInput(nodes(r+indexDiff._1)(c+indexDiff._2), inOrder, s"input_${edgeDirection}_${r+indexDiff._1}_${c+indexDiff._2}($inOrder)", Seq(Schedule(0, 1)))
                inoutNodeAttrSeq += (s"input_${edgeDirection}_${r+indexDiff._1}_${c+indexDiff._2}($inOrder)" -> List(r+indexDiff._1, c+indexDiff._2, inOrder, indexDiff._1, indexDiff._2))
              }
              case _ => logger.error("Wrong InOut type")
            }
          } else if (currInRange && !nextInRange) {
            // current node in range but next node not in range, use current node to do withInput or do withOutput
            InOut match {
              case IN => if (withInput) {
                //println(s"curr ($r, $c) for Input")
                this.setInput(nodes(r)(c), inOrder, s"input_${edgeDirection}_${r}_${c}($inOrder)", Seq(Schedule(0, 1)))
                inoutNodeAttrSeq += (s"input_${edgeDirection}_${r}_${c}($inOrder)" -> List(r, c, inOrder, indexDiff._1, indexDiff._2))
              }
              case OUT => if (withOutput) {
                //println(s"curr ($r, $c) for Output")
                this.setOutput(nodes(r)(c), outOrder, s"output_${edgeDirection}_${r}_${c}($outOrder)", Seq(Schedule(0, 1)))
                inoutNodeAttrSeq += (s"output_${edgeDirection}_${r}_${c}($outOrder)" -> List(r, c, outOrder, indexDiff._1, indexDiff._2))
              }
              case _ => logger.error("Wrong InOut type")
            }
          }
        }  // for col
      }  // for row
    } // def addAllEdgeChain

    // adding all edges
    edges.foreach { edgeGroup =>
      addAllEdgeChain(edgeGroup.edgeDirection, edgeGroup.InOut, edgeGroup.outOrder, edgeGroup.inOrder,
        edgeGroup.delay, edgeGroup.withInput, edgeGroup.withOutput
      )
    }

    println()
    println(this)  // print all Nodes and Edges


    // Visualization
    // use this website https://edotor.net/ and choose engine "neato" to view the graph online
    // see: https://jgrapht.org/guide/UserOverview#:~:text=to%20i%3A%0Anull-,Graph%20Serialization%20and%20Export/Import,-The%20default%20graph
    // TODO: put them in a function
    import scala.compat.java8.FunctionConverters._
    import scala.collection.mutable
    import scala.collection.JavaConverters._
    import java.io.StringWriter

    //println(inoutNodeAttrSeq.mkString("\n"))    // Attribute sequences

    // for Node shape
    def shapeAttr(label:String):String = {
      var shape = ""
      if (label.startsWith("input")) {
        shape = "rect"
      } else if (label.startsWith("output")) {
        shape = "rect"
      } else {
        shape = "doubleoctagon"
      }
      shape
    }

    // for Node color
    def colorAttr(label:String):String = {
      var color = ""
      if (label.startsWith("input")) {
        color = "#00BB00"    // green
      } else if (label.startsWith("output")) {
        color = "#FF0000"    // red
      }
      color
    }

    // FIXME: double inoutput in the same direction (make it prettier maybe?)
    // for Node position
    def posAttr(label:String):String = {
      var pos = ""
      val orderStepping = false    // true or false
      val step = 3    // used to measure the length of unit step in Graphviz placement
      if (label.startsWith("input") || label.startsWith("output")) {
        var inoutList = inoutNodeAttrSeq(label)
        var (x, y, order, index1, index2) = (inoutList(0), inoutList(1), inoutList(2), inoutList(3), inoutList(4))
        if (label.startsWith("input")) {
          if (orderStepping) {
            pos = (((index2*(order+1)) * (step) + y * step * 2).toString + ","
                 + ((index1*(order+1)) * (-step) + x * (-step * 2)).toString + "!")
          } else {
            pos = ((index2 * (step) + y * step * 2).toString + ","
                 + (index1 * (-step) + x * (-step * 2)).toString + "!")
          }
        } else if (label.startsWith("output")) {
          if (orderStepping) {
            pos = (((index2*(order+1)) * (-step) + y * step * 2).toString + ","
                 + ((index1*(order+1)) * (step) + x * (-step * 2)).toString + "!")
          } else {
            pos = ((index2 * (-step) + y * step * 2).toString + ","
                 + (index1 * (step) + x * (-step * 2)).toString + "!")
          }
        }
      } else {
        var posNum = label.split("_").takeRight(2)    // original position: (i, j)
        pos = (posNum(1).toInt * step * 2).toString + "," + (posNum(0).toInt * (-step * 2)).toString + "!"
      }
      pos
    }

    // for edge visibility
    def edgeInvisAttr(edgeName: String): String = {
      var invis = ""
      if (edgeName.contains("Chainsaw.DFG.LatencyEdge")) {
        invis = "invis"    // those LatencyEdges should be invisible
      }
      invis
    }

    // for edge color
    def edgeColorAttr(e: DSPEdge[T]):String = {
      var color = ""
      if (e.delay > 0) {
        color = "#1E90FF"
      } else if (e.source.toString.startsWith("input")) {
        color = "#00BB00"    // green
      } else if (e.target.toString.startsWith("output")) {
        color = "#FF0000"    // red
      }
      color
    }


    val exporter = new DOTExporter[DSPNode[T], DSPEdge[T]]()

    // Setting Nodes Attributes
    def dotNodeAttr(v: DSPNode[T]) = {
      val map = mutable.Map[String, org.jgrapht.nio.Attribute]().asJava
      map.put("label", DefaultAttribute.createAttribute(v.toString))
      map.put("shape", DefaultAttribute.createAttribute(shapeAttr(v.toString)))
      map.put("color", DefaultAttribute.createAttribute(colorAttr(v.toString)))
      map.put("pos", DefaultAttribute.createAttribute(posAttr(v.toString)))
      //println(map)
      map
    }
    val nodeAttrFunction = (v: DSPNode[T]) => {dotNodeAttr(v)}
    exporter.setVertexAttributeProvider(nodeAttrFunction.asJava)

    // Setting Edges Attributes
    def dotEdgeAttr(e: DSPEdge[T]) = {
      val map = mutable.Map[String, org.jgrapht.nio.Attribute]().asJava
      map.put("style", DefaultAttribute.createAttribute(edgeInvisAttr(e.toString)))
      map.put("label", DefaultAttribute.createAttribute(s" z = ${e.delay} "))
      map.put("color", DefaultAttribute.createAttribute(edgeColorAttr(e)))
      map.put("penwidth", DefaultAttribute.createAttribute(s"1"))                 // edge linewidth
      map
    }
    val edgeAttrFunction = (e: DSPEdge[T]) => {dotEdgeAttr(e)}
    exporter.setEdgeAttributeProvider(edgeAttrFunction.asJava)


    val writer = new StringWriter()
    exporter.exportGraph(this, writer)
    println("\nDOT:\n")
    System.out.println(writer.toString)

  }
}

// Systolic Array structure testing
object RegularDFG {
  def main(args: Array[String]): Unit = {
    val dfg = new RegularDFG[Bits]("examplRegularGraph")
    val and: BinaryNode[Bits] = BinaryHardware(Operators.and).asDeviceNode("and")
    val EdgeSeq: Seq[RegularEdge] = Seq(
      RegularEdge(edgeDirection = UP, InOut = IN, outOrder = 0, inOrder = 0, delay = 1, withInput = true, withOutput = true),
      RegularEdge(edgeDirection = LEFT, InOut = IN, outOrder = 1, inOrder = 1, delay = 1, withInput = true, withOutput = true)
      //RegularEdge(edgeDirection = UPRIGHT, InOut = IN, outOrder = 2, inOrder = 2, delay = 1, withInput = true, withOutput = true)
    )
    dfg.build2D(3, 4, and, EdgeSeq)
  }
}


// TODO 2: Try to do test
import org.scalatest.flatspec.AnyFlatSpec
import Chainsaw.matlabIO._
import Chainsaw.ChainsawRand
import Chainsaw.RandomUtil
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class RegularDFGTest extends AnyFlatSpec {

  /*
  val SATestCase: Seq[BigInt] = Seq.fill(10)(BigInt(0)) ++ (0 until 20).map(_ => DSPRand.nextBigInt(2))
  println(s"SATestCase: $SATestCase")
  val SAGolden = SATestCase.map(_ * 2)  //.toArray.map(_.toDouble)
  println(s"SAGolden:   $SAGolden")  //.toList
   */

  def testSA(): Unit = {

    val EdgeSeq: Seq[RegularEdge] = Seq(
      RegularEdge(edgeDirection = UP, InOut = IN, outOrder = 0, inOrder = 0, delay = 1, withInput = true, withOutput = true),
      RegularEdge(edgeDirection = LEFT, InOut = IN, outOrder = 1, inOrder = 1, delay = 1, withInput = true, withOutput = true)
    )
    //val SAdfg = new RegularDFG[Bits]("examplRegularGraph")
    //val and: BinaryNode[Bits] = BinaryHardware(Operators.and).asDeviceNode("and")
    //SAdfg.build2D(1, 1, and, EdgeSeq)

    val SAdfg = new RegularDFG[SInt]("examplRegularGraph")
    val cell = Operators.SACell(6 bits)
    SAdfg.build2D(2, 2, cell, EdgeSeq)
    println("Input and Output Nodes:")
    println(SAdfg.inputNodes)
    println(SAdfg.outputNodes)
    println()
    testDFG(dfg = SAdfg, graphLatency = 2 cycles, inputWidths = Seq.fill(4)(6 bits),        // graphLatency = (row-1) + (col-1)
      testCases = Seq(Seq.fill(4)(BigInt(1))), golden = Seq(0,4,0,4), initLength = 0        // decide golden by yourself
    )

  }

  "SA" should "be correct as simplified SA" in testSA    // Systolic Array testing


  def testMapping() = {
    // TODO 1: Matrix P -> d -> directionMap
    val P = DenseMatrix((0,-1,1),(-1,1,0))
    val dA = DenseVector(0,1,0)
    val dB = DenseVector(1,0,0)
    val dC = DenseVector(0,0,1)
    val A = P * dA    // Matrix Multiplication
    val B = P * dB    // Matrix Multiplication
    val C = P * dC    // Matrix Multiplication
    println(s"\nP * dA = \n$A")
    println(s"\nP * dB = \n$B")
    println(s"\nP * dC = \n$B")
  }

  "SA" should "be correct as SA mapping" in testMapping    // Unfinished mapping

}

