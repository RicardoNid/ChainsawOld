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


import scala.util.Random

val randGen = new Random(42)

val randSeq = (0 until 10).map(i => randGen.nextInt % 100)

val positive = randSeq.map(_ - randSeq.min)
positive.foreach(println)
val avg1 = positive.take(5).sum/5
val avg2 = positive.takeRight(5).sum/5

