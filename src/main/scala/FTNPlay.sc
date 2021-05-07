import scala.collection.mutable.ListBuffer

val table = ListBuffer.fill(10)(0)

(0 until 3).foreach(i => table(i) += 1)

table

