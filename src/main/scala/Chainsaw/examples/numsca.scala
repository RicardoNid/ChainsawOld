//package Chainsaw.examples
//
//
//
//import botkop.{numsca => ns}
//
//
//object numsca {
//  def main(args: Array[String]): Unit = {
//    val x = ns.array(0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1).reshape(4, 3)
//    val y = ns.array(0, 1, 1, 0).T
//    val w0 = 2 * ns.rand(3, 4) - 1
//    val w1 = 2 * ns.rand(4, 1) - 1
//    for (j <- 0 until 60000) {
//      val l1 = 1 / (1 + ns.exp(-ns.dot(x, w0)))
//      val l2 = 1 / (1 + ns.exp(-ns.dot(l1, w1)))
//      val l2Delta = (y - l2) * (l2 * (1 - l2))
//      val l1Delta = l2Delta.dot(w1.T) * (l1 * (1 - l1))
//      w1 += l1.T.dot(l2Delta)
//      w0 += x.T.dot(l1Delta)
//    }
//  }
//}
