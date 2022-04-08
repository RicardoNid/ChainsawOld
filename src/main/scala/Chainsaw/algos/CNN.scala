package Chainsaw.algos

import breeze.linalg._
import breeze.numerics.ceil
import breeze.stats.mean

object CNN {

  type FeatureData = Seq[DenseMatrix[Double]] // 3-d tensor
  type KernelData = Seq[Seq[DenseMatrix[Double]]] // 4-d tensor

  case class DataSize(channel: Int, height: Int, width: Int)

  case class KernelSize(channelOut: Int, channelIn: Int, height: Int, width: Int)

  def getSize(featureData: FeatureData) = DataSize(featureData.length, featureData.head.rows, featureData.head.cols)

  def getSize(kernelData: KernelData) = KernelSize(kernelData.length, kernelData.head.length, kernelData.head.head.rows, kernelData.head.head.cols)

  def convolutionLayer(featureMaps: FeatureData, kernels: KernelData, step: Int) = {
    kernels.map { kernelsOneChannel =>
      val newFeatures: Seq[DenseMatrix[Double]] = featureMaps.zip(kernelsOneChannel).map { case (data, kernel) =>
        convolution2D(data, kernel, step)
      }
      newFeatures.reduce(_ + _) //each channel's sum adds
    }
  }

  def convolution2D(data: DenseMatrix[Double], kernel: DenseMatrix[Double], step: Int) = {
    // out strategy requires that the output size is input size divided by step, and this automatically determines the padding size
    // requirements
    require(data.rows == data.cols) // square matrix only
    require(kernel.rows == kernel.cols)

    val (inputSize, kernelSize) = (data.rows, kernel.rows)
    require(inputSize % step == 0) //step==1

    val outputSize = inputSize / step //padding-same
    //    val paddingSize = (outputSize - 1) * step + kernelSize - inputSize
    val paddingSize = kernelSize - step
    require(paddingSize % 2 == 0)

    // preparing data(padding)
    val paddedSize = inputSize + paddingSize //
    val paddedData = DenseMatrix.zeros[Double](paddedSize, paddedSize) //all zero
    val dataRange = paddingSize / 2 until (paddedSize - (paddingSize / 2)) // 0 until 2 => 0,1
    paddedData(dataRange, dataRange) := data

    // convolution(sliding)
    val output = DenseMatrix.zeros[Double](outputSize, outputSize)

    Seq.tabulate(outputSize, outputSize) { (i, j) =>
      println(s"current output position: $i, $j")
      val rangeX = i * step until (i * step + kernelSize)
      val rangeY = j * step until (j * step + kernelSize)
      val slice = paddedData(rangeX, rangeY)
      output(i, j) = sum(slice *:* kernel) //each kernel * and +
    }
    output
  }

  def depthwiseConvolutionLayer(featureMaps: FeatureData, kernels: KernelData, pointKernel: KernelData, step: Int): FeatureData = {

    val dataSize = getSize(featureMaps)
    val kernelSize = getSize(kernels)
    val pointKernelSize = getSize(pointKernel)

    require(dataSize.channel == kernelSize.channelIn && kernelSize.channelOut == 1) //one channel kernel
    require(getSize(pointKernel).channelIn == dataSize.channel)
    require(pointKernelSize.channelIn == dataSize.channel && pointKernelSize.width == 1 && pointKernelSize.height == 1) //1*1*c

    val midFeatures: FeatureData = featureMaps.zip(kernels.head).map { case (featureMap, kernel) => convolution2D(featureMap, kernel, step) } // depthwise
    val newFeatures = convolutionLayer(midFeatures, pointKernel, 1) // pointwise

    newFeatures
  }

  def fullyConnectLayer(featureMaps: FeatureData, kernels: KernelData) = {

    val dataSize = getSize(featureMaps)
    val kernelSize = getSize(kernels)

    require(dataSize.height == 1 && dataSize.width == 1)
    require(kernelSize.height == 1 && kernelSize.width == 1)
    require(kernelSize.channelIn == dataSize.channel)

    convolutionLayer(featureMaps, kernels, 1)
  }

  def relu6(value: Double) = if (value > 6) 6 else if (value < 0) 0 else value

  def excitation(value: Double) = relu6(value + 3) / 6

  def globalAvgPooling(featureData: FeatureData): DenseVector[Double] = new DenseVector(featureData.map(mean(_)).toArray)

  def excitationBlock(dataIn: DenseVector[Double], fcWeight0: DenseMatrix[Double], fcWeight1: DenseMatrix[Double]) = {

    val afterFc0 = fc(dataIn, fcWeight0)
    val afterEx0 = afterFc0.map(excitation)
    val afterFc1 = fc(afterEx0, fcWeight1)
    val afterEx1 = afterFc1.map(excitation)

    afterEx1
  }

  def fc(dataIn: DenseVector[Double], weight: DenseMatrix[Double]) = weight * dataIn

  def bn(featureMaps: FeatureData, a: Double, b: Double): FeatureData = {
    featureMaps.map(featureMap => featureMap.map(x => a * x + b))
  }

  def seBlock(featureMaps: FeatureData, fcWeight0: DenseMatrix[Double], fcWeight1: DenseMatrix[Double]): FeatureData = {
    val afterS: DenseVector[Double] = globalAvgPooling(featureMaps)
    excitationBlock(afterS, fcWeight0, fcWeight1).toArray.map(data => new DenseMatrix(1, 1, Array(data)))
  }

  def nonLinearLayer(dataIn: FeatureData, excitationMode: Int): FeatureData = {
    val op: Double => Double = if (excitationMode == 1) relu6 else excitation
    dataIn.map(_.map(op))
  }

  def bneck(featureData: FeatureData,
            kernelData0: KernelData, // weight for convolution layer
            kernelData1: KernelData, pointKernel: KernelData, // weight for depth wise convolution layer
            fcWeight0: DenseMatrix[Double], fcWeight1: DenseMatrix[Double], // weight for fc layer in se block
            fcWeight2: KernelData, // weight for fc layer
            excitationMode: Int, seMode: DenseMatrix[Double], step: Int) = {
    val conv1: FeatureData = convolutionLayer(featureData, kernelData0, step)
    val t1: FeatureData = nonLinearLayer(conv1, excitationMode)
    val conv2: FeatureData = depthwiseConvolutionLayer(t1, kernelData1, pointKernel, step)
    val t2: FeatureData = nonLinearLayer(conv2, excitationMode)
    val t3: FeatureData = if (seMode == 1) seBlock(t2, fcWeight0, fcWeight1) else t2
    fullyConnectLayer(t3, fcWeight2)
  }

  def genKernel(a: Int, b: Int, c: Int, d: Int): KernelData = {
    Seq.tabulate(a, b)((_, _) => DenseMatrix.rand[Double](c, d))
  }

  class MobileNetv3_Large(featureData: FeatureData,
                          kernelData0: KernelData, // weight for convolution layer
                          kernelData1: KernelData, pointKernel: KernelData, // weight for depth wise convolution layer
                          fcWeight0: DenseMatrix[Double], fcWeight1: DenseMatrix[Double], // weight for fc layer in se block
                          fcWeight2: KernelData, // weight for fc layer
                          excitationMode: Int, seMode: DenseMatrix[Double], step: Int) {
    val conv1: FeatureData = convolutionLayer(featureData, kernelData0, step)
    val hs1: FeatureData = nonLinearLayer(conv1, excitationMode)
    //    val output1 = {
    //      bneck(Seq(new DenseMatrix(112, 112)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 1, 3, 1)
    //      bneck(Seq(new DenseMatrix(112, 112)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 1, 3, 2)
    //      bneck(Seq(new DenseMatrix(56, 56)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 1, 3, 1)
    //      bneck(Seq(new DenseMatrix(56, 56)), Seq.tabulate(5, 5) { (_, _) => new DenseMatrix(5, 5) }, 1, 1, 2)
    //      bneck(Seq(new DenseMatrix(28, 28)), Seq.tabulate(5, 5) { (_, _) => new DenseMatrix(5, 5) }, 1, 1, 1)
    //      bneck(Seq(new DenseMatrix(28, 28)), Seq.tabulate(5, 5) { (_, _) => new DenseMatrix(5, 5) }, 1, 1, 1)
    //      bneck(Seq(new DenseMatrix(28, 28)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 0, 3, 2)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 0, 3, 1)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 0, 3, 1)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 0, 3, 1)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 0, 1, 1)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(3, 3) { (_, _) => new DenseMatrix(3, 3) }, 0, 1, 1)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(5, 5) { (_, _) => new DenseMatrix(5, 5) }, 0, 1, 1)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(5, 5) { (_, _) => new DenseMatrix(5, 5) }, 0, 1, 2)
    //      bneck(Seq(new DenseMatrix(14, 14)), Seq.tabulate(5, 5) { (_, _) => new DenseMatrix(5, 5) }, 0, 1, 1)
    //    }
    //    val output2 = fullyConnectLayer(output1, Seq.tabulate(1, 1) { (_, _) => new DenseMatrix(1, 1) })
    //    val output3 = output2.map(excitation)
    //    val output4 = globalAvgPooling(output3)
    //    val output5 = output4.map(excitation)
    //    val output6 = fullyConnectLayer(output5, Seq.tabulate(1, 1) { (_, _) => new DenseMatrix(1, 1) })
    //  }

  }
}

object MobileNetv3_Large {

  def main(args: Array[String]): Unit = {
//    val data = new DenseMatrix(12, 12, Array.range(0, 144).map(_.toDouble))
//    val kernel = new DenseMatrix(3, 3, Array.range(0, 9).map(_.toDouble))
//    val output = convolution2D(data, kernel, 3)
//    val featureData: FeatureData = Seq.fill(3)(data)
//    val kernelData: KernelData = Seq.tabulate(4, 3) { (_, _) => kernel }
//    val result = convolutionLayer(featureData, kernelData, 3)
//    println(result.size)
//    println(result.head.rows)
//    println(result.head.cols)
  }
}
