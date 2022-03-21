package Chainsaw.examples

import scala.collection.mutable.ArrayBuffer

object CloneExamples extends App {

  val h = ArrayBuffer(1, 2, 3)
  val g = h
  val i = h.clone()
  h += 4
  println(g) // 1,2,3,4
  println(i)

}
