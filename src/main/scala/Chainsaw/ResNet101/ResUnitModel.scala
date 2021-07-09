package Chainsaw.ResNet101

import ai.djl.ndarray.types.Shape
import ai.djl.ndarray.{NDList, NDManager}
import ai.djl.nn._
import ai.djl.nn.convolutional.Conv2d
import ai.djl.nn.norm.BatchNorm
import ai.djl.training.ParameterStore
import ai.djl.util.PairList


// please notice: the actual nn model is class Residual, written in Java
class ResUnitModel {
  val residual = new Residual(32, false, new Shape(2, 2))
}

object ResUnitModel {
  def main(args: Array[String]): Unit = {
//    println(Residual.getExample.get(0).toArray.mkString(" "))
    // the shape of NDList is NCHW
    val results = Residual.getExample
    val result0 = Residual.getExample.get(0).get(0)
    println(s"shape: ${result0.getShape}")
    val h = result0.getShape.get(1)
    val w = result0.getShape.get(2)
    val formatted = result0.toArray.grouped(h.toInt * w.toInt).toSeq.map(_.grouped(3).toSeq.map(_.mkString(" ")).mkString("\n")).mkString("\n\n")
    println(formatted)
  }
}