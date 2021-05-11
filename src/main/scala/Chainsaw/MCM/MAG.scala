package Chainsaw.MCM

import org.jgrapht.graph.{DefaultEdge, _}
import org.jgrapht.nio.dot.DOTExporter

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Path(fundamentals: ListBuffer[Int] = ListBuffer(1)) {
  def apply(index: Int) = fundamentals(index)

  def +=(fundamental: Int) = fundamentals += fundamental

  def +(fundamental: Int) = {
    val ret = Path(fundamentals.clone())
    ret += fundamental
    ret
  }

  def des = fundamentals.last

  override def toString: String = fundamentals.mkString(" -> ")
}

object MAGBest {

  val exporter = new DOTExporter[Int, DefaultEdge]()

  type AG = DirectedMultigraph[Int, DefaultEdge]
  def copy(graph: AG) = graph.clone().asInstanceOf[AG]
  implicit val max = 1 << 16

  def getPositiveOddFundamental(n: Int) = {
    require(n != 0)
    var ret = if (n < 0) -n else n //  positive
    while (ret % 2 == 0) ret /= 2 // odd
    ret
  }

  val goldenCostLUT = Source.fromFile("src/main/resources/mag14.dat").getLines().mkString("").zipWithIndex
    .map { case (char, i) => (i + 1) -> char.asDigit }
    .filter(pair => pair._1 % 2 != 0)
    .toMap

  implicit class AOperations(u: Int) {
    def AOp(v: Int)(implicit max: Int): mutable.Set[Int] = {
      require(u > 0 && v > 0 && u % 2 != 0 && v % 2 != 0) //  positive odd fundamentals
      val ret = mutable.Set[Int]() //  reachable coefficients
      var exp = 1
      var continue = true
      while (continue) { //  situation 1 & 2, j = 0, k > 0 or j >0, k = 0
        val cands = Array( //  1 << exp stands for 2^i
          (1 << exp) * u + v, (1 << exp) * u - v, v - (1 << exp) * u,
          (1 << exp) * v + u, (1 << exp) * v - u, u - (1 << exp) * v)
        val validCand = cands.filter(cand => cand > 0 && cand <= max)
        validCand.foreach(ret += _)
        continue = validCand.map(_ * 2).exists(_ < max)
        exp += 1
      }
      ret += getPositiveOddFundamental(u + v) //  situation 3, j = k < 0
      if (u != v) ret += getPositiveOddFundamental(if (u > v) u - v else v - u)
      ret
    }
  }

  implicit class MAGOp(graph: AG) {
    def apply(n: Int) = graph.vertexSet().toSeq.filter(_ == n).head

    def addVertex(m: Int, n: Int) = {
      val vertex = graph.vertexSet().size()
      graph.addVertex(vertex)
      graph.addEdge(m, vertex)
      graph.addEdge(n, vertex)
    }
  }

  def apply() = {
    val costLUT = mutable.Map[Int, Int]()

    def infer(graph: AG) = {
      val vertices = 1 until graph.vertexSet().size()
      var paths = ListBuffer(Path())
      vertices.foreach { vertex =>
        val srcs = graph.incomingEdgesOf(vertex).toSeq.map(graph.getEdgeSource(_))
        require(srcs.length == 2)
        //        println(paths.mkString("\n"))
        paths = paths.map(path => (path(srcs(0)) AOp path(srcs(1))).toSeq.map(path + _)).flatten
      }
      paths
    }

    val cost0 = new AG(classOf[DefaultEdge])
    cost0.addVertex(0)
    val cost0Paths = infer(cost0)
    cost0Paths.foreach(path => costLUT.getOrElseUpdate(path.des, 0))

    val cost1 = copy(cost0)
    cost1.addVertex(0, 0)
    val cost1Paths = infer(cost1)
    cost1Paths.foreach(path => costLUT.getOrElseUpdate(path.des, 1))

    val cost2s = ListBuffer.fill(2)(copy(cost1))
    cost2s(0).addVertex(0, 1)
    cost2s(1).addVertex(1, 1)
    val cost2Paths = cost2s.map(infer).flatten
    cost2Paths.foreach(path => costLUT.getOrElseUpdate(path.des, 2))

    val cost3s = ListBuffer[AG]()
    // pattern 1 ~ 6
    (0 until 2).foreach(i => (0 until 3).foreach(_ => cost3s += copy(cost2s(i))))
    for (i <- 0 until 2; j <- 0 until 3) cost3s(i * 3 + j).addVertex(j, 2)
    // pattern 7
    cost3s += copy(cost1)
    cost3s(6).addVertex(0, 0)
    cost3s(6).addVertex(1, 2)

    val cost3Paths = cost3s.map(infer).flatten
    cost3Paths.foreach(path => costLUT.getOrElseUpdate(path.des, 3))

    println(costLUT.size)

    val cost4s = ListBuffer[AG]()
    // pattern 1 ~ 24
    (0 until 6).foreach(i => (0 until 4).foreach(_ => cost4s += copy(cost3s(i))))
    for (i <- 0 until 6; j <- 0 until 4) cost4s(i * 4 + j).addVertex(j, 3)
    // pattern 25 ~ 27
    (0 until 3).foreach(_ => cost4s += copy(cost3s(6)))
    cost4s(24).addVertex(0, 3)
    cost4s(25).addVertex(1, 3)
    cost4s(26).addVertex(3, 3)
    // pattern 28 ~ 32
    val specials = ListBuffer(copy(cost2s(0)), copy(cost2s(1)), copy(cost2s(0)), copy(cost2s(0)), copy(cost2s(1)))
    specials(0).addVertex(0, 0)
    specials(1).addVertex(0, 0)
    specials(2).addVertex(0, 1)
    specials(3).addVertex(1, 1)
    specials(4).addVertex(1, 1)
    specials.foreach(_.addVertex(2, 3))
    cost4s ++= specials
    println(s"cost4s has ${cost4s.length} patterns")

    val cost4Paths = cost4s.map(infer).flatten
    cost4Paths.foreach(path => costLUT.getOrElseUpdate(path.des, 4))

    println(costLUT.size)
    println(costLUT.keySet.max)
    println(costLUT.keySet.forall(coeff => !goldenCostLUT.contains(coeff) || costLUT(coeff) == goldenCostLUT(coeff)))

    //    val writer = new StringWriter()
    //    exporter.exportGraph(cost4s(0), writer)
    //    val graphString = writer.toString
    //    val importer = new DOTImporter[Int, DefaultEdge]()
    //    val graphEmpty = new AG()
    //    val intSupplier = new Supplier[Int] {
    //      private var id = 0
    //      override def get(): Int = {
    //        val ret = id
    //        id += 1
    //        ret
    //      }
    //    }
    //    graphEmpty.setVertexSupplier(intSupplier)
    //    val reader = new StringReader(graphString)
    //    importer.importGraph(graphEmpty, reader)
    //    println(graphEmpty)
  }

  def main(args: Array[String]): Unit = {
    MAGBest()
  }
}
