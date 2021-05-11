//package Chainsaw.MCM
//
//import Chainsaw.MCM.RAGn.getPositiveOddFundamental
//import org.jgrapht.graph.{DefaultEdge, _}
//
//import scala.collection.JavaConversions._
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//import scala.io.Source
//
//case class MAGVertex(id: Int, var coeffs: mutable.Set[Int] = mutable.Set[Int]())
//
//object MAGBetter {
//  type AG = DirectedMultigraph[MAGVertex, DefaultEdge]
//  def copy(graph: AG) = graph.clone().asInstanceOf[AG]
//
//  val goldenCostLUT = Source.fromFile("src/main/resources/mag14.dat").getLines().mkString("").zipWithIndex
//    .map { case (char, i) => (i + 1) -> char.asDigit }
//    .filter(pair => pair._1 % 2 != 0)
//    .toMap
//
//  def ASet(u: Int, v: Int, max: Int): mutable.Set[Int] = {
//    require(u > 0 && v > 0 && u % 2 != 0 && v % 2 != 0) //  positive odd fundamentals
//    val ret = mutable.Set[Int]() //  reachable coefficients
//    var exp = 1
//    var continue = true
//    while (continue) { //  situation 1 & 2, j = 0, k > 0 or j >0, k = 0
//      val cands = Array( //  1 << exp stands for 2^i
//        (1 << exp) * u + v, (1 << exp) * u - v, v - (1 << exp) * u,
//        (1 << exp) * v + u, (1 << exp) * v - u, u - (1 << exp) * v)
//      val validCand = cands.filter(cand => cand > 0 && cand <= max)
//      validCand.foreach(ret += _)
//      continue = validCand.map(_ * 2).exists(_ < max)
//      exp += 1
//    }
//    ret += getPositiveOddFundamental(u + v) //  situation 3, j = k < 0
//    if (u != v) ret += getPositiveOddFundamental(if (u > v) u - v else v - u)
//    ret
//  }
//
//  def ASetOnSets[T <: Iterable[Int]](U: T, V: T, max: Int, symmetric: Boolean = true) = {
//    val ret = mutable.Set[Int]() //  generated coefficietn -> set of (u,v) s
//    if (symmetric) U.foreach(u => ret ++= ASet(u, u, max))
//    else for (u <- U; v <- V) ret ++= ASet(u, v, max)
//    ret
//  }
//
//  implicit class MAGOp(graph: AG) {
//    def apply(n: Int) = graph.vertexSet().toSeq.filter(_.id == n).head
//
//    def addVertex(m: Int, n: Int) = {
//      val vertex = MAGVertex(graph.vertexSet().size())
//      graph.addVertex(vertex)
//      graph.addEdge(graph(m), vertex)
//      graph.addEdge(graph(n), vertex)
//      val generated = ASetOnSets(graph(m).coeffs, graph(n).coeffs, 1 << 14, m == n)
//      vertex.coeffs = generated
//      println(generated.size)
//    }
//  }
//
//  def apply() = {
//    val costLUT = ListBuffer[mutable.Set[Int]]()
//
//    def checkLUT(i: Int) = {
//      println(costLUT(i).size)
//      println(costLUT(i).forall(coeff => goldenCostLUT.get(coeff).get == i))
//      println(costLUT(i).map(coeff => goldenCostLUT.get(coeff).get))
//    }
//
//    val cost0 = new AG(classOf[DefaultEdge])
//    cost0.addVertex(MAGVertex(0))
//    cost0(0).coeffs += 1
//    costLUT += cost0(0).coeffs
//
//    val cost1 = copy(cost0)
//    cost1.addVertex(0, 0)
//    costLUT += (cost1(1).coeffs -- costLUT(0))
//    checkLUT(1)
//
//    val cost2 = ListBuffer(copy(cost1), copy(cost1))
//    cost2(0).addVertex(0, 1)
//    cost2(1).addVertex(1, 1)
//    costLUT += (cost2.map(graph => graph(2).coeffs).reduce(_ ++ _) -- costLUT.reduce(_ ++ _))
//
//    checkLUT(2)
//
//    val cost3 = ListBuffer[AG]()
//    (0 until 3).foreach(_ => cost3 += copy(cost2(0)))
//    (0 until 3).foreach(_ => cost3 += copy(cost2(1)))
//    //    cost3 += copy(cost1)
//    cost3(0).addVertex(0, 2)
//    cost3(1).addVertex(1, 2)
//    cost3(2).addVertex(2, 2)
//    cost3(3).addVertex(0, 2)
//    cost3(4).addVertex(1, 2)
//    cost3(5).addVertex(2, 2)
//    //    cost3(6).addVertex(0, 0)
//    //    println(cost3(6)(2))
//    //    cost3(6).addVertex(1,2)
//    costLUT += (cost3.map(graph => graph(3).coeffs).reduce(_ ++ _) -- costLUT.reduce(_ ++ _))
//    checkLUT(3)
//  }
//
//  def main(args: Array[String]): Unit = {
//    MAGBetter()
//  }
//}
