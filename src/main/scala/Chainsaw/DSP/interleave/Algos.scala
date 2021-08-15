package Chainsaw.DSP.interleave

object Algos {
  def matIntrlv[T](input: Seq[T], row: Int, col: Int): Seq[T] = Seq.tabulate(col, row)((i, j) => j * col + i).flatten.map(input(_))
  def matIntrlv1D2D[T](input: Seq[T], row: Int, col: Int): Seq[Seq[T]] = Seq.tabulate(col, row)((i, j) => j * col + i).map(_.map(input(_)))
  def matIntrlv2D2D[T](input: Seq[Seq[T]], row: Int, col: Int) = Seq.tabulate(col, row)((i, j) => input(j)(i)) // transpose
  def matIntrlv2D1D[T](input: Seq[Seq[T]], row: Int, col: Int) = Seq.tabulate(col, row)((i, j) => input(j)(i)).flatten
}
