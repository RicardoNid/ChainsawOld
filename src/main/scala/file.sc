import scala.io.Source

val filename = "/home/lsfans/LTRSpinal/src/main/scala/mag14.dat"
val goldenCostLUT = Source.fromFile(filename).getLines().mkString("")
  .zipWithIndex.map { case (c, i) => (i + 1) -> c }.toMap

goldenCostLUT.size
goldenCostLUT.get(1151).get
