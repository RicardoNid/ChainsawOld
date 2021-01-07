package DJLExamples

import ai.djl.Application
import ai.djl.Device
import ai.djl.MalformedModelException
import ai.djl.Model
import ai.djl.ModelException
import ai.djl.basicdataset.Cifar10
import ai.djl.basicmodelzoo.BasicModelZoo
import ai.djl.basicmodelzoo.cv.classification.ResNetV1
import ai.djl.inference.Predictor
import ai.djl.metric.Metrics
import ai.djl.modality.Classifications
import ai.djl.modality.cv.Image
import ai.djl.modality.cv.ImageFactory
import ai.djl.modality.cv.transform.Normalize
import ai.djl.modality.cv.transform.ToTensor
import ai.djl.modality.cv.translator.ImageClassificationTranslator
import ai.djl.ndarray.types.Shape
import ai.djl.nn.Block
import ai.djl.nn.Blocks
import ai.djl.nn.SequentialBlock
import ai.djl.nn.SymbolBlock
import ai.djl.nn.core.Linear
import ai.djl.repository.zoo.Criteria
import ai.djl.repository.zoo.ModelNotFoundException
import ai.djl.repository.zoo.ModelZoo
import ai.djl.repository.zoo.ZooModel
import ai.djl.training.DefaultTrainingConfig
import ai.djl.training.EasyTrain
import ai.djl.training.Trainer
import ai.djl.training.TrainingResult
import ai.djl.training.dataset.Dataset
import ai.djl.training.dataset.RandomAccessDataset
import ai.djl.training.evaluator.Accuracy
import ai.djl.training.listener.TrainingListener
import ai.djl.training.loss.Loss
import ai.djl.training.util.ProgressBar
import ai.djl.translate.Pipeline
import ai.djl.translate.TranslateException
import java.io.IOException
import java.nio.file.Path
import java.nio.file.Paths
import java.util

import ai.djl.ndarray._
import ai.djl.ndarray.types._
import org.slf4j.Logger
import org.slf4j.LoggerFactory


// https://docs.djl.ai/examples/index.html

// how to find a pretrained model
// insight family + criteria,表格可以在下面页面查到
// https://docs.djl.ai/mxnet/mxnet-model-zoo/index.html

/*
example:
  val criteria = mutable.HashMap[String, String]()
  criteria.put("layers", "101")
  criteria.put("flavor", "v1")
  criteria.put("dataset", "imagenet")

  val model = MxModelZoo.RESNET.loadModel(criteria)
 */

object Example2 {
  def main(args: Array[String]): Unit = {
    def getModel = {
      import ai.djl.Model
      val manager = NDManager.newBaseManager()
      val model = Model.newInstance("resnetv1")
      val builder = ResNetV1.builder()
      val resNet50 = builder
        .setImageShape(new Shape(3, 32, 32))
        .setNumLayers(50)
        .setOutSize(10)
        .build()
      resNet50.initialize(manager, DataType.INT32, new Shape(1, 3, 32, 32))
      model.setBlock(resNet50)
      println(resNet50.getChildren.get(0).getValue)
    }

    getModel
  }
}
