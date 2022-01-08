package Chainsaw.algos

object Permutations {

  def matIntrlv[T](input: Seq[T], row: Int, col: Int): Seq[T] = Seq.tabulate(col, row)((i, j) => j * col + i).flatten.map(input(_))

  def transpose[T](input: Seq[Seq[T]]): Seq[Seq[T]] = {
    val row = input.size
    val col = input.head.size
    Seq.tabulate(col, row)((i, j) => input(j)(i)) // transpose
  }

}
