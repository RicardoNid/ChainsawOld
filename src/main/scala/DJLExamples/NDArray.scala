package DJLExamples

import ai.djl.ndarray._
import ai.djl.ndarray.index._
import ai.djl.ndarray.types._

object NDArray {
  def main(args: Array[String]): Unit = {
    val manager = NDManager.newBaseManager()
    val nd1 = manager.create(Array.tabulate(2, 3)(_ + _))
    val nd2 = manager.create(Array.tabulate(3, 2)(_ * _))

    println(nd1.getShape)
    println(nd1.getShape.dimension())
    println(nd1.get(new NDIndex("0:2:1, 0"))) // nd1(0:2:1, 0)
  }
}
