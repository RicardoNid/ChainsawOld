package Chainsaw.MCM

import Chainsaw.Architectures.BinarySFG
import Chainsaw.MCM.AOperations._

import java.io._
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import scala.io.Source

object MAG {

  def apply(n: Int) = {
    val (path, graphString) = getOnePathLUT(n)
    rebuildMAG(path, graphString)
  }

  implicit val max = 1 << 16
  val costLUTFileName = s"src/main/resources/16bitsCostLUT.dat"
  val pathLUTFileName = s"src/main/resources/16bitsPathLUT.dat"
  val onePathLUTFileName = s"src/main/resources/16bitsOnePathLUT.dat"

  case class Path(fundamentals: ListBuffer[Int] = ListBuffer(1)) {
    def apply(index: Int) = fundamentals(index)

    def +=(fundamental: Int) = fundamentals += fundamental

    def +(fundamental: Int) = {
      val ret = Path(fundamentals.clone())
      ret += fundamental
      ret
    }

    def des = fundamentals.last

    def cost = fundamentals.length - 1

    override def toString: String = fundamentals.mkString(" -> ")
  }

  def searchMAG(graph: BinarySFG) = {
    val vertices = 1 until graph.vertexSet().size()
    var paths = ListBuffer(Path())
    vertices.foreach { vertex =>
      val drivers = graph.driversOf(vertex)
      require(drivers.length == 2)
      paths = paths.flatMap(path => (path(drivers(0)) AOp path(drivers(1))).toSeq.map(path + _))
    }
    paths
  }

  /**
   * @see [[https://www.notion.so/RAG-n-e772eb87d24841c8abc2042533f00eda Chainsaw RAG-n - MAG paper Fig 4]]
   */
  def topologies = {
    // structure of cost 0 graphs
    val cost0 = new BinarySFG
    cost0.addVertex(0)
    // structure of cost 1 graphs
    val cost1 = cost0.copy
    cost1.addVertex(0, 0)
    // structure of cost 2 graphs
    val cost2s = ListBuffer.fill(2)(cost1.copy)
    cost2s(0).addVertex(0, 1)
    cost2s(1).addVertex(1, 1)
    // structure of cost 3 graphs
    val cost3s = ListBuffer[BinarySFG]()
    // pattern 1 ~ 6
    (0 until 2).foreach(i => (0 until 3).foreach(_ => cost3s += cost2s(i).copy))
    for (i <- 0 until 2; j <- 0 until 3) cost3s(i * 3 + j).addVertex(j, 2)
    // pattern 7
    cost3s += cost1.copy
    cost3s(6).addVertex(0, 0)
    cost3s(6).addVertex(1, 2)
    // structure of cost 4 graphs
    val cost4s = ListBuffer[BinarySFG]()
    // pattern 1 ~ 24
    (0 until 6).foreach(i => (0 until 4).foreach(_ => cost4s += cost3s(i).copy))
    for (i <- 0 until 6; j <- 0 until 4) cost4s(i * 4 + j).addVertex(j, 3)
    // pattern 25 ~ 27
    (0 until 3).foreach(_ => cost4s += cost3s(6).copy)
    cost4s(24).addVertex(0, 3)
    cost4s(25).addVertex(1, 3)
    cost4s(26).addVertex(3, 3)
    // pattern 28 ~ 32
    val specials = ListBuffer(cost2s(0).copy, cost2s(1).copy, cost2s(0).copy, cost2s(0).copy, cost2s(1).copy)
    specials(0).addVertex(0, 0)
    specials(1).addVertex(0, 0)
    specials(2).addVertex(0, 1)
    specials(3).addVertex(1, 1)
    specials(4).addVertex(1, 1)
    specials.foreach(_.addVertex(2, 3))
    cost4s ++= specials

    ListBuffer(ListBuffer(cost0), ListBuffer(cost1), cost2s, cost3s, cost4s)
  }

  def filesReady = Array(costLUTFileName, pathLUTFileName, onePathLUTFileName).forall(new File(_).exists())

  def getCostLUT =
    if (filesReady) new ObjectInputStream(new FileInputStream(costLUTFileName))
      .readObject.asInstanceOf[mutable.Map[Int, Int]]
    else buildLUTs()._1

  def getPathLUT =
    if (filesReady) new ObjectInputStream(new FileInputStream(onePathLUTFileName))
      .readObject.asInstanceOf[mutable.Map[Int, ListBuffer[(Path, String)]]]
    else buildLUTs()._2

  def getOnePathLUT =
    if (filesReady) new ObjectInputStream(new FileInputStream(onePathLUTFileName))
      .readObject.asInstanceOf[mutable.Map[Int, (Path, String)]]
    else buildLUTs()._3

  def rebuildMAG(path: Path, mag: BinarySFG) = {
    val magInfos = (0 until mag.size).map { vertex =>
      import AOpSign.ADD
      val drivers = mag.driversOf(vertex)
      if (drivers.isEmpty) AOpConfig(0, 0, 0, ADD)
      else {
        val (left, right) = (drivers(0), drivers(1))
        AReverse(path(vertex), path(left), path(right))
      }
    }
    (mag, magInfos)
  }

  def rebuildMAG(path: Path, graphString: String): (BinarySFG, immutable.IndexedSeq[AOpConfig]) = rebuildMAG(path, BinarySFG.fromSerialized(graphString))

  def buildLUTs(): (mutable.Map[Int, Int], mutable.Map[Int, ListBuffer[(Path, String)]], mutable.Map[Int, (Path, String)]) = {

    val goldenCostLUT = Source.fromFile("src/main/resources/mag14.dat").getLines().mkString("").zipWithIndex
      .map { case (char, i) => (i + 1) -> char.asDigit }
      .filter(pair => pair._1 % 2 != 0)
      .toMap

    val pathLUT = mutable.Map[Int, ListBuffer[(Path, String)]]()
    val costLUT = mutable.Map[Int, Int]()

    def buildCostLUT(graphs: ListBuffer[BinarySFG], cost: Int) = {
      graphs.flatMap(searchMAG)
        .foreach(path => costLUT.getOrElseUpdate(path.des, cost))
    }

    def buildPathLUT(graphs: ListBuffer[BinarySFG], cost: Int): Unit = {
      graphs.foreach { graph =>
        val graphString = graph.serialized
        val paths = searchMAG(graph)
        paths.foreach { path =>
          val coeff = path.des
          if (costLUT(coeff) == cost) {
            pathLUT.getOrElseUpdate(coeff, ListBuffer[(Path, String)]())
            pathLUT(coeff) += (path -> graphString)
          }
        }
      }
    }

    val ooss = Array(costLUTFileName, pathLUTFileName, onePathLUTFileName).map(filepath =>
      new ObjectOutputStream(new FileOutputStream(filepath)))

    (0 to 4).foreach { i =>
      println(s"building cost-$i graphs")
      buildCostLUT(topologies(i), i)
      println(s"${costLUT.size} coeffs found")
      buildPathLUT(topologies(i), i)
    }

    val onePathLUT = pathLUT.map { case (i, tuples) =>
      val pathCosts = tuples.map(_._1).map(_.fundamentals.sum)
      val index = pathCosts.indexOf(pathCosts.min)
      i -> tuples(index)
    }

    println("graphs building done")
    println("strat writing graphs")

    ooss(0).writeObject(costLUT)
    ooss(1).writeObject(pathLUT)
    ooss(2).writeObject(onePathLUT)

    ooss.foreach(_.close())

    println("graphs writing done")

    (costLUT, pathLUT, onePathLUT)
  }

  def main(args: Array[String]): Unit = {
    println(MAG(1859))
    println(getOnePathLUT(13)._1)
    println(getPathLUT(12345))
  }
}
