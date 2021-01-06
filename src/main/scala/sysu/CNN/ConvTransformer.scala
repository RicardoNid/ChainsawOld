//package sysu.CNN
//
//import spinal.core._
//import spinal.lib._
//import spinal.lib.fsm._
//import spinal.lib.bus.amba4.axi._
//import spinal.core.sim._
//
//case class ConvTransformer(
//                            definition: Map[LoopVar, Array[Int]] = Map(
//                              OF -> Array(0, 0),
//                              IF -> Array(0, 0),
//                              OY -> Array(0, 0),
//                              OX -> Array(0, 0),
//                              KY -> Array(0, 0),
//                              KX -> Array(0, 0)
//                            )
//                          )(implicit config: LoopNestConv) {
//
//  import config._
//
//  // order的表示上,让用户使用枚举量传入,然后在内部使用默认顺序下的index表示
//  val tilePre = Array(EMPTY) ++ tileOrder.dropRight(1)
//  val tileNext2Pre = tileOrder.zip(tilePre).map{
//    case(pre, next) => pre -> next
//  }.toMap
//
//  def access(): Unit = {
//
//  }
//
//  def generateTile() = { // 生成一个tile上的计数信号
//
//    val counters = Array(Tkx, Tky, Tox, Toy, Tif, Tof).map(Counter(_))
//    Array(KX,KY,OX,OY,IF,OF).foreach{ele =>
//      val pre = tileNext2Pre(ele)
//      val preIndex = tileOrder.indexOf(pre)
//      val eleIndex = tileOrder.indexOf(ele)
//      if(pre != EMPTY) when(counters(preIndex).willOverflow)(counters(eleIndex).increment())
//      else counters(eleIndex).increment()
//    }
//
//    val output = counters(0) * U(definition(KX)(0)) + Counter(definition(KX)(1), counters(0).willOverflow)
//  }
//}
