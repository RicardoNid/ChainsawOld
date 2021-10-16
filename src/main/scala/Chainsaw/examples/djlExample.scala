package Chainsaw.examples

import ai.djl.Application
import ai.djl.basicmodelzoo.BasicModelZoo
import ai.djl.modality.Classifications
import ai.djl.ndarray.NDList
import ai.djl.ndarray.types.Shape
import ai.djl.nn.Block
import ai.djl.translate.{Batchifier, Translator, TranslatorContext}


case class IrisFlower(var sepalLength: Float, var sepalWidth: Float, var petalLength: Float, var petalWidth: Float)

import java.util

class MyTranslator() extends Translator[IrisFlower, Classifications] { // species name

  final private var synset = util.Arrays.asList("setosa", "versicle", "virginica")

  override def processInput(ctx: TranslatorContext, input: IrisFlower): NDList = {
    val data = Array(input.sepalLength, input.sepalWidth, input.petalLength, input.petalWidth)
    val array = ctx.getNDManager.create(data, new Shape(1, 4))
    new NDList(array)
  }

  override def processOutput(ctx: TranslatorContext, list: NDList) = new Classifications(synset, list.get(1))

  override def getBatchifier: Batchifier = null
}



object MyTranslator {


  def main(args: Array[String]): Unit = {

    import ai.djl.nn.convolutional.Conv2d
    import ai.djl.nn.Parameter

    val model: Block = ai.djl.basicmodelzoo.cv.classification.VGG.builder()
      .setNumLayers(16)
      .build()
    println(model)
    println(model.getChildren.get(0).getValue)
    println(model.getChildren.get(0).getValue.getChildren.get(0).getValue)

    val conv2d_1 = model.getChildren.get(0).getValue.getChildren.get(0).getValue.asInstanceOf[Conv2d]
  }

}
