package Chainsaw.algos

object PartialSum {

  def segmentSum(connect: Array[Boolean], data: Array[Int]) = {

    require(data.length == connect.length + 1)

    def recursiveTask(segment: Array[Boolean], data: Array[Int]): Array[Int] = {
      val n = data.length
      if (n == 2) {
        val sum = data(0) + data(1)
        if (connect.head)  Array(sum, sum)
        else Array(data(0), data(1))
      }
      else {
        val half = n/2
        val retLeft: Array[Int] = recursiveTask(segment.take(half), data.take(half))
        val retRight: Array[Int] = recursiveTask(segment.takeRight(half), data.takeRight(half))
        val sum = retLeft.last + retRight.head
        if(segment(half)) retLeft ++ retRight
        else retLeft.init ++ Array.fill(2)(sum)  ++ retRight.tail
      }
    }

    recursiveTask(connect, data)
  }

  def main(args: Array[String]): Unit = {
    val segment = Array(1,0,1,0,1,0,1).map(_ == 1)
    val data = 0 until 8
    println(segmentSum(segment, data.toArray).mkString(" "))
  }

}
