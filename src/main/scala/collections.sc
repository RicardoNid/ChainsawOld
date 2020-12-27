import scala.collection.mutable.ArrayBuffer

val array1 = Array(1,2,3,2)
val array2 = Array(1,1,2,1,3)
val array3 = Array[Int]()
//
array1.contains(1)
array1.containsSlice(Array(2))
array1.startsWith(Array(2,3))
array1.endsWith(Array(3,2))
array1.indexOf(2)
array1.indexOfSlice(Array(3,2))
array1.indexWhere(_ == 2)
array1.lastIndexOf(2)
array1.lastIndexOfSlice(Array(3,2))
array1.lastIndexWhere(_ == 2)
array1.count(_ == 2)

array1.exists(_ == 3)
array1.forall(_ < 4)

array1.max
array1.min
array1.sum
array1.product

array1.segmentLength(_ > 1, 1)

array1.isEmpty
array1.nonEmpty
array1.size
array1.length
array1.hasDefiniteSize
array1.isDefinedAt(10)

array1.foldLeft(1)(_ + _)
array1.foldRight(1)(_ + _)
array1.reduceLeft(_ + _)
array1.reduceRight(_ + _)

val plus1 = new PartialFunction[Int, Int] {
  override def apply(x: Int): Int = x + 1
  override def isDefinedAt(x: Int): Boolean = x > 0 && x < 2
}
array1.collect(plus1)
array1.filter(_ > 2)
array1.filterNot(_ > 2)
array1.take(3)
array1.takeRight(2)
array1.takeWhile(_ > 2) // 基于segmentLength
array1.drop(3)
array1.dropRight(2)
array1.dropWhile(_ > 2) // 基于segmentLength
array1.slice(0,2)

array1.find(_ == 2)
array1.head
array3.headOption
array1.tail
array1.last
array3.lastOption
array1.init

array1.distinct
array1.intersect(array2)
array1.union(array2)
array1.diff(array2) // 有点微妙

val set1 = Set(1,2,3)
val set2 = Set(2,3,4)
set1.intersect(set2)
set1.union(set2)
set1.diff(set2)

array1 ++ array2
//array1 +
//array1 -
//array1 --
array1.map(_ + 1)
array1.flatMap(ele => Array(ele, ele + 1, ele + 2))
array1.reverse
array1.sorted
array1.sortBy(_ % 2)
array1.sortWith(_ > _)
array1.zip(array2)
//array1.zipAll()
array1.zipWithIndex

array1.groupBy(_ % 2)
array1.grouped(2).toArray
array1.partition(_ > 2)
val iter = array1.sliding(2,2)
for(ele <- iter) println(ele.mkString(" "))
array1.span(_ < 3)
array1.splitAt(3)
array1.zip(array2).unzip

//array1.view()
array1.par(3)
Array(array1, array2).flatten
array1.foreach(println(_))
array1.mkString(" ")

// immutable
array1 ++ array2
array1 :+ 1
1 +: array1


