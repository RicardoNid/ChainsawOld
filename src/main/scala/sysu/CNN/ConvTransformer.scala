package sysu.CNN

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

case class ConvTransformer(
                            definition: Map[String, Array[Int]] = Map(
                              "OF" -> Array(0, 0, 1),
                              "IF" -> Array(0, 0, 1),
                              "OY" -> Array(0, 0, 1),
                              "OX" -> Array(0, 0, 1),
                              "KY" -> Array(0, 0, 1),
                              "KX" -> Array(0, 0, 1)
                            )
                          )(implicit config: LoopNestConv) {

  import config._

  val tilePre = Array(EMPTY) ++ tileOrder.dropRight(1)
  val tileNext2Pre = tileOrder.zip(tilePre).map{
    case(pre, next) => pre -> next
  }.toMap

  def access(): Unit = {

  }

  def generateTile() = {

    //
    val counters = Array(Tkx, Tky, Tox, Toy, Tif, Tof).map(Counter(_))
    Array(KX,KY).foreach{ele =>
      if(tileNext2Pre(ele) != EMPTY){
        when()
      }
    }
  }
}
