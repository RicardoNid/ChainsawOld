val matrix = Array.tabulate(13, 13)(_ * 13 + _)
println(matrix.map(_.mkString(" ")).mkString("\n"))

def conv(input: Array[Array[Int]], K: Int, S: Int) = {
  require(K % 2 == 1)
  require(input.size == input(0).size)
  require((input.size - K) % S == 0)
  val hOut = (input.size - K) / S + 1
  input
    .map(_.sliding(K, S).toArray) // 滑窗到三维
    .sliding(K, S).toArray // 滑窗到四维
    .map(_.flatten.zipWithIndex.sortBy { case (ele, i) => i % hOut }.map { case (ele, i) => ele }.grouped(K).toArray) // 交换2/3维
    .map(_.map(_.flatten))
    .flatten
}

val result = conv(matrix, 3, 2)
result.length
result(0).length
result.map(_.mkString(" ")).mkString("\n")

// 在大量函数的帮助下,你几乎不需要写任何流程控制语句





