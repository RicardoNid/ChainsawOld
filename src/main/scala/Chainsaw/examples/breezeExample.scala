package Chainsaw.examples

import Chainsaw.core.Fixed
import breeze.linalg.DenseVector

object breezeExample {
  def main(args: Array[String]): Unit = {

    val mat = DenseVector.fill(5)(Fixed(5, true, 10, 5))
    println(mat + mat)

  }
}
