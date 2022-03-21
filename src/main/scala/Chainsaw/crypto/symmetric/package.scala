package Chainsaw.crypto

package object symmetric {

  // substituation and permutation are basic methods in symmetric cryptography
  def substitution[T](data: Seq[Int], table: Seq[Int]): Seq[Int] = data.map(index => table(index))

  def permutation[T](data: Seq[T], order: Seq[Int]): Seq[T] = {
    order.map(index => data(index - 1))
  }

  def inversePermutation[T](data: Seq[T], order: Seq[Int]): Seq[T] = {
    val inverseOrder = (0 +: order).zipWithIndex.sortBy(_._1).map(_._2).drop(1) // so smart!
    permutation(data, inverseOrder)
  }

  /** get BigInts from HEX strings split by spaces
    */
  def getBigSeqFromHex(string: String): Array[BigInt] = string.split(" +").map(string => BigInt(string, 16))

}
