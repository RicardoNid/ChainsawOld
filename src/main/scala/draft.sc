import scala.collection.mutable.ArrayBuffer

// mutable
val buf1 = ArrayBuffer(1, 2, 3, 2)
val buf2 = ArrayBuffer(1, 1, 2, 2)

buf1 += 1

buf1 += (1, 2, 3)
buf1 ++= buf2

buf1 -= 1
buf1 -= (1, 2, 3)
buf1 --= buf2
buf1 --= buf2
buf1(0) = 4
buf1.clear()
buf1
buf1 ++= buf2
buf1.remove(2)
buf1
buf1.remove(0, 2)
buf1

buf1 ++ buf2

import scala.collection.mutable.{Map => MMap}

val m1 = Map[Int, Int](1 -> 1, 2 -> 2, 3 -> 3)
val m2 = Map[Int, Int]()
val mm1 = MMap[Int, Int]()
val mm2 = MMap[Int, Int]()

m1 + (1 -> 1)
m1 - 1
m1 - (1, 2)