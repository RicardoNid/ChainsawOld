package Chainsaw.algos

object Interpolation {

  def getPixels(image: BMPImage, x: Int, y: Int) = {
    val ret = new Array[Short](3)
    var xIdx, yIdx = 0
    if(x > image.header.height_px - 1)  {xIdx = image.header.height_px - 1}else if(x < 0){xIdx = 0} else{xIdx = x}
    if(y > image.header.width_px - 1 )   {yIdx = image.header.width_px - 1 }else if(y < 0){yIdx = 0} else{yIdx = y}
    ret(0) = image.pixels.r(yIdx + xIdx * image.header.width_px)
    ret(1) = image.pixels.g(yIdx + xIdx * image.header.width_px)
    ret(2) = image.pixels.b(yIdx + xIdx * image.header.width_px)
    ret
  }

  def BiQuadratic(srcWidth: Long, srcHeight: Long,
                  tagWidth: Long, tagHeight: Long,
                  fileName: String, filePath: String = "/home/lqx/bmpFile") = {
    // load source bmp image
    val srcBmp = BMPImage(srcWidth, srcHeight)
    srcBmp.bmpRead(fileName, filePath)
    // generate target bmp image class
    val tagBmp = BMPImage(tagWidth, tagHeight)

    // do interpolation
    val yDoubleScale = srcWidth.toDouble / tagWidth.toDouble
    val xDoubleScale = srcHeight.toDouble / tagHeight.toDouble

    (0 until tagBmp.header.height_px).foreach{ x =>
      val xDPosition = x * xDoubleScale
      val xIPosition = xDPosition.toInt
      val xDiff = xDPosition - xIPosition
      (0 until tagBmp.header.width_px).foreach{ y =>
        val yDPosition = y * yDoubleScale
        val yIPosition = yDPosition.toInt
        val yDiff = yDPosition - yIPosition

        val coffs = Array.ofDim[Double](2, 2)
        coffs(0)(0) = (1.toDouble - xDiff) * (1.toDouble - yDiff)
        coffs(0)(1) = xDiff * (1.toDouble - yDiff)
        coffs(1)(0) = xDiff * yDiff
        coffs(1)(1) = (1.toDouble - xDiff) * yDiff

        tagBmp.pixels.r(y + x * tagBmp.header.width_px) = (coffs(0)(0) * getPixels(srcBmp, xIPosition, yIPosition)(0)
          + coffs(0)(1) * getPixels(srcBmp, xIPosition + 1, yIPosition)(0)
          + coffs(1)(0) * getPixels(srcBmp, xIPosition + 1, yIPosition + 1)(0)
          + coffs(1)(1) * getPixels(srcBmp, xIPosition, yIPosition + 1)(0)).toShort
        tagBmp.pixels.g(y + x * tagBmp.header.width_px) = (coffs(0)(0) * getPixels(srcBmp, xIPosition, yIPosition)(1)
          + coffs(0)(1) * getPixels(srcBmp, xIPosition + 1, yIPosition)(1)
          + coffs(1)(0) * getPixels(srcBmp, xIPosition + 1, yIPosition + 1)(1)
          + coffs(1)(1) * getPixels(srcBmp, xIPosition, yIPosition + 1)(1)).toShort
        tagBmp.pixels.b(y + x * tagBmp.header.width_px) = (coffs(0)(0) * getPixels(srcBmp, xIPosition, yIPosition)(2)
          + coffs(0)(1) * getPixels(srcBmp, xIPosition + 1, yIPosition)(2)
          + coffs(1)(0) * getPixels(srcBmp, xIPosition + 1, yIPosition + 1)(2)
          + coffs(1)(1) * getPixels(srcBmp, xIPosition, yIPosition + 1)(2)).toShort
      }

    }
    tagBmp
  }
}

object TestInterpolation extends App{
  Interpolation.BiQuadratic(960L, 540L, 3840L, 2160L, "downscaled.bmp").bmpWrite("upscaled.bmp")
}