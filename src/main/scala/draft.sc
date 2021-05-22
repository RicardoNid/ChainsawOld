import scala.collection.mutable.ListBuffer

val temp = Seq(1,2,3,4)
val temp1 = temp :+ 5

temp
temp1

temp ++ temp1

val temp2 = ListBuffer(1,2,3,4)
temp2 += 5
temp2
val temp3 = ListBuffer(1,2,3)
temp2 ++= temp3
temp3

