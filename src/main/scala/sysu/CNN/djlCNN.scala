package sysu.CNN

import ai.djl._
import ai.djl.ndarray._
import ai.djl.nn._
import ai.djl.nn.core._
import ai.djl.training._
import java.nio.file._
import ai.djl._
import ai.djl.basicdataset._
import ai.djl.ndarray.types._
import ai.djl.training._
import ai.djl.training.dataset._
import ai.djl.training.initializer._
import ai.djl.training.loss._
import ai.djl.training.listener._
import ai.djl.training.evaluator._
import ai.djl.training.optimizer._
import ai.djl.training.util._
import ai.djl.basicmodelzoo.cv.classification._
import ai.djl.basicmodelzoo.basic._
import ai.djl.mxnet.zoo.MxModelZoo

class djlCNN {

}

object djlCNN {
  def main(args: Array[String]): Unit = {
    val inputSize = 28 * 28
    val outputSize = 10

    val block = new SequentialBlock()
    block.add(Blocks.batchFlattenBlock(inputSize))
    block.add(Linear.builder.setUnits(128).build)
    block.add(Activation.reluBlock())
    block.add(Linear.builder.setUnits(64).build)
    block.add(Activation.reluBlock())
    block.add(Linear.builder.setUnits(outputSize).build)

    import ai.djl.nn.convolutional.Conv2d
    val manager = NDManager.newBaseManager()
    val input = manager.create(Array.tabulate(1,8,9,9)(_ * 648 + _ * 81 + _ * 9 + _).flatten.flatten).reshape(1,8,9,9).toType(DataType.FLOAT32, false)
    val weight =  manager.create(Array.tabulate(16,8,3,3)(_ + _ + _ + _).flatten.flatten).reshape(16,8,3,3).toType(DataType.FLOAT32, false)
    println(Conv2d.conv2d(input, weight).get(0).toType(DataType.INT32, false).toIntArray.mkString(" "))
  }

}
