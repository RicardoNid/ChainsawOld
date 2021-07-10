package Chainsaw.ResNet101

import ai.djl.basicmodelzoo.cv.classification.ResNetV1
import ai.djl.ndarray.types.{DataType, Shape}
import ai.djl.ndarray.{NDArray, NDList, NDManager}
import ai.djl.nn._
import ai.djl.training.ParameterStore

import scala.collection.JavaConversions._


// please notice: the actual nn model is class Residual, written in Java
object ResUnitModel {
  def main(args: Array[String]): Unit = {

    def runABlock() = {

      // a residualBlock from ResNetV1
      val residualBlock = ResNetV1.residualUnit(
        16,
        new Shape(1, 1),
        true,
        true,
        0.0f
      )

      val manager = NDManager.newBaseManager // always, we need a manager to provide a context
      val X = manager.randomUniform(0f, 1.0f, new Shape(4, 16, 14, 14)) // randomized input
      System.out.println(X.getDataType) // float32 by default
      val parameterStore = new ParameterStore(manager, true)

      residualBlock.initialize(manager, DataType.FLOAT32, X.getShape) // initialize the weight
      val result = residualBlock.forward(parameterStore, new NDList(X), false) // "run" the residualBlock, do calculation

      val shape = result.singletonOrThrow.getShape
      println(shape)
      val batch0 = result.get(0) // NCHW array
      val array0: NDArray = batch0.get(0) // CHW array
      val sizes = (0 until 3).map(i => array0.getShape().get(i))
      val C = sizes(0).toInt
      val H = sizes(1).toInt
      val W = sizes(2).toInt

      def func(num:Double) = num.toString.take(4)
      (num:Double) => num.toString.take(4)

      val formatted = array0.toArray.grouped(H * W).take(4)  // separate the feature maps, and only take four of them
        .map(_.grouped(W).map(_.map(_.toString.take(4)).mkString(" ")).mkString("\n")).mkString("\n\n")

      println(formatted)
    }

    def buildAModel() = {
      val resnet101 = ResNetV1.builder()
        .setNumLayers(101)
        .setImageShape(new Shape(3, 224, 224))
        .setOutSize(10l)
        .optBatchNormMomentum(0.0f)
        .build()

      // these are two examples, showing that how children still have children
      println(resnet101.getChildren.toSeq.map(_.getKey).mkString("\n"))
      // show the block05
      println(resnet101.getChildren.toSeq(4).getValue.getChildren.toSeq.map(_.getKey).mkString(" "))
    }

    // TODO: write a recursive algo that can print the hierarchy of layers of the model, like the hierarchy of file system
    def modelSummary(block: Block) = {

    }

    runABlock()
    buildAModel()
  }
}