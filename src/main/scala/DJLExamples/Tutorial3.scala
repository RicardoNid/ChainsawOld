package DJLExamples

import java.nio.file._
import java.awt.image._
import java.nio.file._
import java.util._
import java.util.function.IntFunction
import java.util.stream._

import ai.djl._
import ai.djl.basicmodelzoo.basic._
import ai.djl.ndarray._
import ai.djl.modality._
import ai.djl.modality.cv._
import ai.djl.modality.cv.util.NDImageUtils
import ai.djl.translate._


object Tutorial3 {
  def main(args: Array[String]): Unit = {

    val img = ImageFactory.getInstance().fromUrl("https://resources.djl.ai/images/0.png")
    img.getWrappedImage

    val modelDir = Paths.get("build/mlp")
    val model = Model.newInstance("mlp")
    model.setBlock(new Mlp(28 * 28, 10, Array(128, 64)))
    model.load(modelDir)

    import ai.djl.modality.Classifications
    import ai.djl.modality.cv.Image
    import ai.djl.modality.cv.util.NDImageUtils
    import ai.djl.ndarray.NDArray
    import ai.djl.ndarray.NDList
    import ai.djl.translate.Batchifier
    import ai.djl.translate.Translator
    import ai.djl.translate.TranslatorContext
    import java.util
    import java.util.stream.Collectors
    import java.util.stream.IntStream
    import scala.collection.JavaConversions._

    val translator = new Translator[Image, Classifications]() {
      override def processInput(ctx: TranslatorContext, input: Image): NDList = { // Convert Image to NDArray
        val array = input.toNDArray(ctx.getNDManager, Image.Flag.GRAYSCALE)
        new NDList(NDImageUtils.toTensor(array))
      }

      override

      def processOutput(ctx: TranslatorContext, list: NDList): Classifications = { // Create a Classifications with the output probabilities
        val probabilities = list.singletonOrThrow.softmax(0)
        val classNames = (0 until 10).map(_.toString).toList
        new Classifications(classNames, probabilities)
      }

      override

      def getBatchifier: Batchifier = { // The Batchifier describes how to combine a batch together
        // Stacking, the most common batchifier, takes N [X1, X2, ...] arrays to a single [N, X1, X2, ...] array
        Batchifier.STACK
      }
    }
    val predictor = model.newPredictor(translator)
    val classifications = predictor.predict(img)
    println(classifications)
  }
}
