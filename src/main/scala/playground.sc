import scala.collection.mutable

def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = {

  var resTable = mutable.Map[Tuple3[Int, Int, Int], Int]()
  var crossTable = mutable.Map[Tuple3[Int, Int, Int], Int]()

  val m = mat.length
  val n = mat(0).length

  for (k <- 0 to K) {
    for (i <- 0 until m; j <- 0 until n) {
      if (k == 0) {
        resTable += ((i, j, k) -> mat(i)(j))
        crossTable += ((i, j, k) -> mat(i)(j))

      }
      else {
        // update crossTable
        val cross = crossTable(i, j, k - 1) +
          resTable.getOrElse((i - k, j, 0), 0) +
          resTable.getOrElse((i + k, j, 0), 0) +
          resTable.getOrElse((i, j + k, 0), 0) +
          resTable.getOrElse((i, j - k, 0), 0)
        crossTable += (i, j, k) -> cross
        //        println(crossTable(i, j, k - 1), resTable.getOrElse((i - k, j, 0), 0), resTable.getOrElse((i + k, j, 0), 0), resTable.getOrElse((i, j - k, 0), 0), resTable.getOrElse((i, j + k, 0), 0))
        //        println()
        // update resTable
        val prev = if (k % 2 == 0) (k / 2) else ((k - 1) / 2)
        println(s"prev: $prev")
        val res = resTable.getOrElse((i - 1, j - 1, prev), 0) +
          resTable.getOrElse((i + 1, j - 1, prev), 0) +
          resTable.getOrElse((i - 1, j + 1, prev), 0) +
          resTable.getOrElse((i + 1, j + 1, prev), 0) +
          crossTable((i, j, k))
        resTable += (i, j, k) -> res
        println(resTable.getOrElse((i - 1, j - 1, prev), 0), resTable.getOrElse((i - 1, j + 1, prev), 0), resTable.getOrElse((i + 1, j - 1, prev), 0), resTable.getOrElse((i + 1, j + 1, prev), 0), crossTable((i, j, k)))
        println()
      }
    }
  }

  Array.tabulate(m, n)(resTable(_, _, K))
}

val test1 = Array.tabulate(3, 3)(_ * 3 + _ + 1)
matrixBlockSum(test1, 2)