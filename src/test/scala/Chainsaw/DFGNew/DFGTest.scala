package Chainsaw.DFGNew

import org.scalatest.flatspec.AnyFlatSpec

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

class DFGTest extends AnyFlatSpec {

  val dfg = DFG[SInt]

  import Operators._

  val pts = (0 until 4).map(i => SIntPT.asDSPNode(s"pt$i", 1 cycle, 1 ns))
  val Seq(pt0, pt1, pt2, pt3) = pts
  dfg.addPath(pt0 >> 1 >> pt1 >> 1 >> pt2 >> 1 >> pt3)
  dfg.setInput(pt0)
  dfg.setOutput(pt3)

  println(dfg.vertexSet().mkString(" "))
  println(dfg.edgeSet().mkString(" "))

  GenRTL(new Component {
    val dataIn = in SInt (4 bits)
    val dataOut = out SInt (4 bits)

    dataOut := dfg.impl(Seq(dataIn)).head
  })

  // fig 6.3
  "dfg" should "fold correctly" in {
    val fig6_3 = DFG[SInt]
    // add vertices
    val adds = (0 until 4).map(i => SIntAdder.asDSPNode(s"add$i", 2 cycle, 2 ns))
    val Seq(adds0, adds1, adds2, adds3) = adds
    val mults = (0 until 4).map(i => SIntMult.asDSPNode(s"mult$i", 2 cycle, 2 ns))
    val Seq(mults0, mults1, mults2, mults3) = mults

    (adds ++ mults).foreach(fig6_3.addVertex(_))
    // drive vertices
    val input = fig6_3.setInput(adds0, "dataIn")
    val exps = Seq(
      adds2 >=> 0 >=> adds0,
      Seq(adds0, adds3) >=> Seq(1, 0) >=> adds1,
      Seq(mults0, mults2) >=> Seq(0, 1) >=> adds2,
      Seq(mults1, mults3) >=> Seq(1, 1) >=> adds3,
      adds0 >=> 1 >=> mults0,
      adds0 >=> 1 >=> mults1,
      adds0 >=> 1 >=> mults2,
      adds0 >=> 2 >=> mults3)
    exps.foreach(fig6_3.addExp(_))
    fig6_3.setOutput(adds1)

    println(fig6_3)
  }

}
