package Chainsaw.BuildingBlocks

import xilinx.VivadoFlow

import scala.io.Source
import scala.sys.process.Process
import Chainsaw.MCM.MAG

import scala.collection.mutable.ListBuffer

// using FloPoCo as backend
class FloPoCo {

}

object FloPoCo {

  val flopocoPath = "/home/ltr/FloPoCo/flopoco/build"

  def main(args: Array[String]): Unit = {
    Process("./flopoco IntConstMult wIn=16 n=101", new java.io.File(flopocoPath)) !
    val file = Source.fromFile(flopocoPath + "/flopoco.vhdl")
    println(file.getLines().toSeq.filter(_.trim.startsWith("-- P")).mkString("\n"))

    val graph = new Chainsaw.Architectures.BinarySFG
    graph.addVertex(0)
    graph.addVertex(0,0)
    graph.addVertex(0,0)
    graph.addVertex(1,2)

    println(MAG.rebuildMAG(MAG.Path(ListBuffer(1, 3, 5, 101)), graph))

    //    VivadoFlow(design = new WallaceTreeDUT, topModuleName = "wallace", workspacePath = "/home/ltr/IdeaProjects/Chainsaw/synthWorkspace/wallace").doit()
  }
}
