import scala.collection.mutable.ListBuffer

def uniquePaths(paths: ListBuffer[ListBuffer[Int]]) = {
  val newPaths = ListBuffer[ListBuffer[Int]]()
  paths.foreach(path => if (newPaths.forall(_ != path)) newPaths += path)
  newPaths
}

val temp = ListBuffer(
  ListBuffer(1, 2, 3),
  ListBuffer(1, 2, 3),
  ListBuffer(2, 3, 4)
)

temp.distinct.size