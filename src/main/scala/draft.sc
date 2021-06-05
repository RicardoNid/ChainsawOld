val temp = Array(
  0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1,
  0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0)

println(temp.size)

val groups = temp.grouped(16).toArray
groups(1).zip(groups(0)).map(pair => Array(pair._1, pair._2)).flatten