package Chainsaw.DSP

import spinal.core._

import scala.collection.JavaConversions._
import scala.language.postfixOps


case class BenesAlgo() {

  def butterfly(a: Int, b: Int, switch: Boolean) = if (switch) Seq(b, a) else Seq(a, b)

  def doBenes(dataIn: Seq[Int], controlIn: Seq[Seq[Boolean]]): Seq[Int] = {
    val n = dataIn.size

    def getUp[T](dataIn: Seq[T]) = dataIn.take(dataIn.size / 2)

    def getBottom[T](dataIn: Seq[T]) = dataIn.takeRight(dataIn.size / 2)

    if (n == 2) butterfly(dataIn(0), dataIn(1), controlIn.head.head)
    else {
      // decompose controlIn
      val (pre, post, mid) = (controlIn.head, controlIn.last, controlIn.drop(1).dropRight(1))
      val (subNetworkUp, subNetworkBottom) = (mid.map(getUp), mid.map(getBottom))
      // build network
      val afterPre = getUp(dataIn).zip(getBottom(dataIn)).zip(pre)
        .map { case ((a, b), switch) => butterfly(a, b, switch) } // switches
      val afterPreOrdered: Seq[Int] = afterPre.map(_.head) ++ afterPre.map(_.last) // connections
      val afterSub = doBenes(getUp(afterPreOrdered), subNetworkUp) ++ doBenes(getBottom(afterPreOrdered), subNetworkBottom) // connections
      val afterPost = getUp(afterSub).zip(getBottom(afterSub)).zip(post)
        .map { case ((a, b), switch) => butterfly(a, b, switch) } // switches
      afterPost.map(_.head) ++ afterPost.map(_.last) // connections
    }
  }

  def getControlFromPermutation(permutation: Seq[Int]): Seq[Seq[Boolean]] = {

    // assert that the permutation is legal
    val n = permutation.size
    require(isPow2(n))
    require(permutation.sorted.equals(permutation.indices))

    if (n == 2) { // base solution
      Seq(Seq(permutation.head == 1)) // for 0,1 -> false, else(1,0) true
    } else {
      // build for graph
      import org.jgrapht.graph._
      val colorGraph = new SimpleGraph[Int, DefaultEdge](classOf[DefaultEdge])
      // nodes
      permutation.indices.foreach(colorGraph.addVertex)
      // input side constraint
      (0 until n / 2).foreach(i => colorGraph.addEdge(i, i + n / 2))
      // output side constraint
      (0 until n / 2).foreach(i => colorGraph.addEdge(permutation(i), permutation(i + n / 2)))

      import org.jgrapht.alg.color.GreedyColoring
      val color = new GreedyColoring(colorGraph).getColoring.getColors.toSeq

      require(color.forall(_._2 < 2)) // 2-color requirement
      val up = color.filter(_._2 == 0).map(_._1)
      val bottom = color.filter(_._2 == 1).map(_._1)

      // sub-permutation
      val problem0 = up.sortBy(permutation.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val problem1 = bottom.sortBy(permutation.indexOf(_) % (n / 2)).map(_ % (n / 2))
      val solutionMid: Seq[Seq[Boolean]] = getControlFromPermutation(problem0)
        .zip(getControlFromPermutation(problem1))
        .map { case (s0, s1) => s0 ++ s1 }

      // solution of this level
      val solutionPre: Seq[Boolean] = up.sortBy(_ % (n / 2)).map(_ >= (n / 2))
      val solutionPost: Seq[Boolean] = up.map(permutation.indexOf(_)).sortBy(_ % (n / 2)).map(_ >= (n / 2))

      // combine solutions
      solutionPre +: solutionMid :+ solutionPost
    }
  }
}

object BenesAlgo extends App {

  val benes = BenesAlgo()
  val data = Seq(0, 1, 2, 3, 4, 5, 6, 7)

  //  val permutation0 = Seq(0, 1, 2, 3, 4, 5, 6, 7)
  //  val control0 = benes.getControlFromPermutation(permutation0)
  //  println(benes.doBenes(data, control0).mkString(" "))

  val permutation1 = Seq(7, 4, 6, 5, 3, 1, 2, 0)
  val control1 = benes.getControlFromPermutation(permutation1)
  println(benes.doBenes(data, control1).mkString(" "))

  // TODO: random permutation test
}