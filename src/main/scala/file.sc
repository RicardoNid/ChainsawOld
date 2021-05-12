import scala.collection.mutable.ListBuffer

val a = ListBuffer(1, 2, 3)
val b = a
val c = a.map(i => i)

b += 4
c += 5

a
