package Chainsaw.Communication.viterbi

case class MinplusMatrix(value: Array[Array[Double]]) {

  import MinplusMatrix._

  def rows = value.length
  def cols = value.head.length
  def apply(i: Int, j: Int) = this.value(i)(j)
  def isDense = value.forall(_.forall(_ < Int.MaxValue / 3))

  override def toString = s"Minplus matrix: \n${this.value.map(_.map(i => if (i >= max) "-" else i.toString).mkString(" ")).mkString("\n")}"

  def *(that: MinplusMatrix) = {
    // this: (i,j), that: (j,k)
    require(this.cols == that.rows)
    val ret = Array.tabulate(this.rows, that.cols) { (i, k) =>
      (0 until this.cols).map(j => this (i, j) + that(j, k)).min // min-plus
    }
    MinplusMatrix(ret)
  }

}

object MinplusMatrix {

  val max = Double.MaxValue / 3

  def trellis2Minplus(trellis: Trellis[Int], metric: (Int, Int) => Double) = {
    import trellis._
    (0 until numOutputSymbols).map { observed =>
      val values = Array.tabulate(numStates, numStates) { (prev, next) =>
        val index = nextStates(prev).indexOf(next)
        if (index == -1) max
        else metric(outputs(prev)(index), observed)
      }
      MinplusMatrix(values)
    }
  }

  def findMid(head: MinplusMatrix, tail: MinplusMatrix, start:Int, end:Int) ={
    require(head.cols == tail.rows)
    (0 until head.cols).map(j => head(start, j) + tail(j, end)).zipWithIndex.minBy(_._1)._2
  }

  def main(args: Array[String]): Unit = {

    val trellis = Trellis.poly2trellis(3, Array(7, 6))
    val matrices = trellis2Minplus(trellis, Algos.Hamming)
    println(matrices.mkString("\n\n"))

    val A1 = matrices(3)
    val A2 = matrices(2)
    val A3 = matrices(3)
    val A4 = matrices(0)
    val A5 = matrices(1)
    val A6 = matrices(2)

    val B1 = Seq(A1, A2, A3).reduce(_ * _)
    val B2 = Seq(A4, A5, A6).reduce(_ * _)

    println(MinplusMatrix.findMid(B1, B2, 0, 0))
  }
}
