package FTN

object FTNSoft {

  def interleave(array: Array[Int], depth: Int) = {
    require(array.length % depth == 0)

    val capacity = array.length
    val interleavedIndexes2D = Array.tabulate(capacity / depth, depth)(_ + _ * capacity / depth)
    val interleavedIndexes1D = interleavedIndexes2D.flatten
    (0 until capacity).map(i => array(interleavedIndexes1D(i)))
  }

  def main(args: Array[String]): Unit = {
    println(interleave((0 until 20).toArray, 4).mkString(" "))
  }
}
