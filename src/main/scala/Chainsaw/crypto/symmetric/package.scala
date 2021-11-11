package Chainsaw.crypto

package object symmetric {

  def permutation[T](data: Seq[T], order: Seq[Int]): Seq[T] = {
    order.map(index => data(index - 1))
  }

  def inversePermutation[T](data: Seq[T], order: Seq[Int]): Seq[T] = {
    val inverseOrder = (0 +: order).zipWithIndex.sortBy(_._1).map(_._2).drop(1) // so smart!
    permutation(data, inverseOrder)
  }

  def substitution[T](data: Seq[Int], table: Seq[Int]) = data.map(index => table(index))

  def getSeqFromText(string: String) = string.split(" +").map(_.toInt)

  def getBigSeqFromHex(string: String) = {
    string.split(" +").map(string => BigInt(string, 16))
  }


}
