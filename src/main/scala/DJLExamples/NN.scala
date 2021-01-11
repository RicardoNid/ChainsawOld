package DJLExamples

import ai.djl._
import ai.djl.nn._
import ai.djl.nn.core._
import ai.djl.nn.convolutional._
import ai.djl.nn.pooling._
import ai.djl.ndarray._
import ai.djl.ndarray.types._

import scala.collection.JavaConversions._
import ai.djl.mxnet.zoo._
import ai.djl.Device._

import ai.djl.training.initializer._

import scala.collection.mutable

class NN {

}

object NN {
  def main(args: Array[String]): Unit = {
    // Declare a model// Declare a model
    val manager = NDManager.newBaseManager()
    //    val model = Model.newInstance("mine")

    // Define primary block

    val mainBlock = new SequentialBlock
    val dropoutProbability = 0.5.toFloat
    val fullyConnected = 1024
    val numberOfFilters = 256

    /**
     * Every block returns a NDList which feeds into the consecutive Block
     * 1D Pooling is defined as a LambdaBlock
     */

    mainBlock
      .add(Conv1d.builder()
        .setKernelShape(new Shape(7))
        .setFilters(numberOfFilters)
        .build())
      .add(Activation.reluBlock())
      .add(Pool.maxPool1dBlock(new Shape(3), new Shape(3), new Shape(0))) // java不能
      .add(Conv1d.builder()
        .setKernelShape(new Shape(7))
        .setFilters(numberOfFilters)
        .build())
      .add(Linear.builder()
        .setUnits(10)
        .build())

    mainBlock.setInitializer(new ConstantInitializer(0.0f))
    mainBlock.initialize(manager, DataType.INT32, new Shape(3, 64, 64))
    //Set the mainblock as model's starting point

    //    model.setBlock(mainBlock)

    mainBlock.getChildren foreach { pair =>
      val block = pair.getValue
      block.describeInput.foreach(pair => println(pair.getValue))
    }
  }
}