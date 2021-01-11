package DJLExamples

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

object Tutorial2 {
  def main(args: Array[String]): Unit = {

    val batchSize = 65
    val mnist = Mnist.builder().setSampling(batchSize, true).build()
    val model = Model.newInstance("nlp")
    model.setBlock(new Mlp(28 * 28, 10, Array(128, 64)))
    val config = new DefaultTrainingConfig(Loss.softmaxCrossEntropyLoss())
      .addEvaluator(new Accuracy())

    val trainer = model.newTrainer(config)
    trainer.initialize(new Shape(1,28,28))

    import ai.djl.training.EasyTrain
    // Deep learning is typically trained in epochs where each epoch trains the model on each item in the dataset once.// Deep learning is typically trained in epochs where each epoch trains the model on each item in the dataset once.

    val epoch = 10

    for (i <- 0 until epoch) {
      val index = 0
      // We iterate through the dataset once during each epoch
      import scala.collection.JavaConversions._ // 这部分代码是自动产生的,这就是JetBrians,无敌
      for (batch <- trainer.iterateDataset(mnist)) { // During trainBatch, we update the loss and evaluators with the results for the training batch.
        EasyTrain.trainBatch(trainer, batch)
        // Now, we update the model parameters based on the results of the latest trainBatch
        trainer.step
        // We must make sure to close the batch to ensure all the memory associated with the batch is cleared quickly.
        // If the memory isn't closed after each batch, you will very quickly run out of memory on your GPU
        batch.close
      }
      println("epoch", i, "finished")
    }

    val modelDir = Paths.get("build/mlp")
    Files.createDirectories(modelDir)
    model.setProperty("Epoch", String.valueOf(epoch))
    model.save(modelDir, "mlp")
    println(model)
  }
}
