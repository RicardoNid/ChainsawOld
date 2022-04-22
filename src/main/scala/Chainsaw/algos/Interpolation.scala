package Chainsaw.algos

import scala.sys.process._
import java.io._
import java.nio.file.Paths
import scala.io._
import PathInfo._
import breeze.linalg._

import scala.math._
import breeze.math._

object Interpolation {

  def getPixels(image: BMPImage, x: Int, y: Int) = {
    case class retPixel(var r: Short, var g: Short, var b: Short){}
    val ret = retPixel(0, 0, 0)
    var xIdx, yIdx = 0
    if(x > image.header.height_px - 1)  {xIdx = image.header.height_px - 1}else if(x < 0){xIdx = 0} else{xIdx = x}
    if(y > image.header.width_px - 1 )   {yIdx = image.header.width_px - 1 }else if(y < 0){yIdx = 0} else{yIdx = y}
    image.height >=0 match {
      case true  => ret.r = image.pixels.r(yIdx + image.header.width_px * (image.header.height_px - xIdx - 1))
        ret.g = image.pixels.g(yIdx + image.header.width_px * (image.header.height_px - xIdx - 1))
        ret.b = image.pixels.b(yIdx + image.header.width_px * (image.header.height_px - xIdx - 1))
      case false => ret.r = image.pixels.r(yIdx + xIdx * image.header.width_px)
        ret.g = image.pixels.g(yIdx + xIdx * image.header.width_px)
        ret.b = image.pixels.b(yIdx + xIdx * image.header.width_px)
    }
    ret
  }

  def BiQuadratic(tagWidth: Long, tagHeight: Long,
                  srcWidth: Long = 960L, srcHeight: Long = 540L,
                  fileName: String, filePath: String = scalaDownPath): BMPImage = {
    // load source bmp image
    val srcBmp = BMPImage(srcWidth, srcHeight)
    srcBmp.bmpRead(fileName, filePath)
    // generate target bmp image class
    val tagBmp = BMPImage(tagWidth, tagHeight)

    // do interpolation
    val yDoubleScale = srcWidth.toDouble / tagWidth.toDouble
    val xDoubleScale = srcHeight.toDouble / tagHeight.toDouble

    (0 until tagBmp.header.height_px).foreach{ x =>
      val xDPosition = x * xDoubleScale + 0.5 * (xDoubleScale - 1)
      val xIPosition = xDPosition.toInt
      val xDiff = xDPosition - xIPosition
      (0 until tagBmp.header.width_px).foreach{ y =>
        val yDPosition = y * yDoubleScale + 0.5 * (yDoubleScale - 1)
        val yIPosition = yDPosition.toInt
        val yDiff = yDPosition - yIPosition

        val coffs = Array.ofDim[Double](2, 2)
        coffs(0)(0) = (1.toDouble - xDiff) * (1.toDouble - yDiff)
        coffs(0)(1) = xDiff * (1.toDouble - yDiff)
        coffs(1)(0) = xDiff * yDiff
        coffs(1)(1) = (1.toDouble - xDiff) * yDiff

        tagBmp.pixels.r(tagBmp.transPosition(x, y)) = (coffs(0)(0) * srcBmp.getPixels(xIPosition, yIPosition).r
          + coffs(0)(1) * srcBmp.getPixels(xIPosition + 1, yIPosition).r
          + coffs(1)(0) * srcBmp.getPixels(xIPosition + 1, yIPosition + 1).r
          + coffs(1)(1) * srcBmp.getPixels(xIPosition, yIPosition + 1).r).toShort
        tagBmp.pixels.g(tagBmp.transPosition(x, y)) = (coffs(0)(0) * srcBmp.getPixels(xIPosition, yIPosition).g
          + coffs(0)(1) * srcBmp.getPixels(xIPosition + 1, yIPosition).g
          + coffs(1)(0) * srcBmp.getPixels(xIPosition + 1, yIPosition + 1).g
          + coffs(1)(1) * srcBmp.getPixels(xIPosition, yIPosition + 1).g).toShort
        tagBmp.pixels.b(tagBmp.transPosition(x, y)) = (coffs(0)(0) * srcBmp.getPixels(xIPosition, yIPosition).b
          + coffs(0)(1) * srcBmp.getPixels(xIPosition + 1, yIPosition).b
          + coffs(1)(0) * srcBmp.getPixels(xIPosition + 1, yIPosition + 1).b
          + coffs(1)(1) * srcBmp.getPixels(xIPosition, yIPosition + 1).b).toShort
      }

    }
    tagBmp
  }

  def getCovFactor(x: Double, a: Double = -0.5) = {
//    println(s"the x is ${x}")
    assert(x.abs <= 2, "The value for getting covFactor is invalid !")
    var ret = 0D
    if(x.abs >= 0 && x.abs <= 1){
       ret = 1 - (a + 3) * pow(x, 2) + (a + 2) * pow(abs(x), 3)
    }else{
       ret = -4 * a + 8 * a * abs(x) - 5 * a * pow(x, 2) + a * pow(abs(x), 3)
    }
    ret
  }

  def BicubicInterpolation(tagWidth: Long, tagHeight: Long,
                           srcWidth: Long = 960L, srcHeight: Long = 540L,
                           fileName: String, filePath: String = scalaDownPath): BMPImage = {
    // load source bmp image
    val srcBmp = BMPImage(srcWidth, srcHeight)
    srcBmp.bmpRead(fileName, filePath)
    // generate target bmp image class
    val tagBmp = BMPImage(tagWidth, tagHeight)
    // get scalar factor
    val yDoubleScale = srcWidth.toDouble / tagWidth.toDouble
    val xDoubleScale = srcHeight.toDouble / tagHeight.toDouble

    (0 until tagBmp.header.height_px).foreach { x =>
      var xDPosition = x * xDoubleScale + 0.5 * (xDoubleScale - 1)
      if(xDPosition < 0) {xDPosition = 0}
      val xIPosition = xDPosition.toInt
      val xDiff = xDPosition - xIPosition
      (0 until tagBmp.header.width_px).foreach { y =>
        var yDPosition = y * yDoubleScale + 0.5 * (yDoubleScale - 1)
        if(yDPosition < 0) {yDPosition = 0}
        val yIPosition = yDPosition.toInt
//        println(s"the xDPosi is ${xDPosition}\t the xIPosi is ${xIPosition}")
        val yDiff = yDPosition - yIPosition
//        println(s"the xDiff is ${xDiff}\t the yDiff is ${yDiff}")
        val rowFactorVector = DenseVector(getCovFactor(1 + xDiff), getCovFactor(xDiff), getCovFactor(1 - xDiff), getCovFactor(2 - xDiff)).t
        val colFactorVector = DenseVector(getCovFactor(1 + yDiff), getCovFactor(yDiff), getCovFactor(1 - yDiff), getCovFactor(2 - yDiff))
        val rPixelsMatrix = DenseMatrix(
          (srcBmp.getPixels(xIPosition - 1, yIPosition - 1).r, srcBmp.getPixels(xIPosition - 1, yIPosition).r,
           srcBmp.getPixels(xIPosition - 1, yIPosition + 1).r, srcBmp.getPixels(xIPosition - 1, yIPosition + 2).r),
          (srcBmp.getPixels(xIPosition, yIPosition - 1).r, srcBmp.getPixels(xIPosition, yIPosition).r,
           srcBmp.getPixels(xIPosition, yIPosition + 1).r, srcBmp.getPixels(xIPosition, yIPosition + 2).r),
          (srcBmp.getPixels(xIPosition + 1, yIPosition - 1).r, srcBmp.getPixels(xIPosition + 1, yIPosition).r,
           srcBmp.getPixels(xIPosition + 1, yIPosition + 1).r, srcBmp.getPixels(xIPosition + 1, yIPosition + 2).r),
          (srcBmp.getPixels(xIPosition + 2, yIPosition - 1).r, srcBmp.getPixels(xIPosition + 2, yIPosition).r,
            srcBmp.getPixels(xIPosition + 2, yIPosition + 1).r, srcBmp.getPixels(xIPosition + 2, yIPosition + 2).r)
        ).map(_.toDouble)
        val gPixelsMatrix = DenseMatrix(
          (srcBmp.getPixels(xIPosition - 1, yIPosition - 1).g, srcBmp.getPixels(xIPosition - 1, yIPosition).g,
            srcBmp.getPixels(xIPosition - 1, yIPosition + 1).g, srcBmp.getPixels(xIPosition - 1, yIPosition + 2).g),
          (srcBmp.getPixels(xIPosition, yIPosition - 1).g, srcBmp.getPixels(xIPosition, yIPosition).g,
            srcBmp.getPixels(xIPosition, yIPosition + 1).r, srcBmp.getPixels(xIPosition, yIPosition + 2).g),
          (srcBmp.getPixels(xIPosition + 1, yIPosition - 1).g, srcBmp.getPixels(xIPosition + 1, yIPosition).g,
            srcBmp.getPixels(xIPosition + 1, yIPosition + 1).g, srcBmp.getPixels(xIPosition + 1, yIPosition + 2).g),
          (srcBmp.getPixels(xIPosition + 2, yIPosition - 1).g, srcBmp.getPixels(xIPosition + 2, yIPosition).g,
            srcBmp.getPixels(xIPosition + 2, yIPosition + 1).g, srcBmp.getPixels(xIPosition + 2, yIPosition + 2).g)
        ).map(_.toDouble)
        val bPixelsMatrix = DenseMatrix(
          (srcBmp.getPixels(xIPosition - 1, yIPosition - 1).b, srcBmp.getPixels(xIPosition - 1, yIPosition).b,
            srcBmp.getPixels(xIPosition - 1, yIPosition + 1).b, srcBmp.getPixels(xIPosition - 1, yIPosition + 2).b),
          (srcBmp.getPixels(xIPosition, yIPosition - 1).b, srcBmp.getPixels(xIPosition, yIPosition).b,
            srcBmp.getPixels(xIPosition, yIPosition + 1).b, srcBmp.getPixels(xIPosition, yIPosition + 2).b),
          (srcBmp.getPixels(xIPosition + 1, yIPosition - 1).b, srcBmp.getPixels(xIPosition + 1, yIPosition).b,
            srcBmp.getPixels(xIPosition + 1, yIPosition + 1).b, srcBmp.getPixels(xIPosition + 1, yIPosition + 2).b),
          (srcBmp.getPixels(xIPosition + 2, yIPosition - 1).b, srcBmp.getPixels(xIPosition + 2, yIPosition).b,
            srcBmp.getPixels(xIPosition + 2, yIPosition + 1).b, srcBmp.getPixels(xIPosition + 2, yIPosition + 2).b)
        ).map(_.toDouble)
        tagBmp.pixels.r(tagBmp.transPosition(x, y)) = (rowFactorVector * rPixelsMatrix * colFactorVector).toShort
        tagBmp.pixels.g(tagBmp.transPosition(x, y)) = (rowFactorVector * gPixelsMatrix * colFactorVector).toShort
        tagBmp.pixels.b(tagBmp.transPosition(x, y)) = (rowFactorVector * bPixelsMatrix * colFactorVector).toShort
      }
    }
    tagBmp
  }


  def isWindows: Boolean = System.getProperty("os.name").toLowerCase().contains("win")

  def doCmd(cmd: String, path: String, logger: String): Int = {
    println(s"***********${logger} Command info**********")
    println(cmd)
    println(s"***********${logger} Output info***********")

    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !
  }

  def getCmdResult(cmd: String, path: String, logger: String): String = {
    println(s"***********${logger} Command info**********")
    println(cmd)
    println(s"***********${logger} Output info***********")

    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !!
    else
      Process(cmd, new java.io.File(path)) !!
  }


  def getCResult(srcBmp: String = "0", cPath: String = cCodePath, logger: String = "Compile C and run upsampling for single bmp"): Unit = {
    val ret = doCmd("make run SRC_BMP=" + srcBmp, cPath, logger)
    if(ret == 0) {
      println("Finish upsampling !")
    }
  }

  def getAllCResult(cPath: String = cCodePath, bmpPath: String = cDownPath): Unit = {
    //get all bmp name in bmpPath
    val fileList = Paths.get(bmpPath).toFile.listFiles().map(_.getPath.replace(bmpPath, "").dropRight(4)).sortWith(_.toInt < _.toInt)
    fileList.foreach(getCResult(_, cPath))
  }

  def verifyCAndScala(srcBmp: String = "0",
                      scalaPath: String = scalaUpPath,
                      cPath: String = cUpPath,
                      logger: String = "verify C and Scala program function"): Unit = {
    getCResult(srcBmp)
    if(doCmd("cmp " + scalaPath + s"${srcBmp}_upsampling.bmp" + " " + cPath + s"${srcBmp}_upsampling.bmp", scalaPath, logger) == 0)
      println(s"The upsampling result of C and Scala for ${srcBmp} is same !")
  }

  def verifyAllCAndScala(scalaPath: String = scalaUpPath,
                         cPath: String = cUpPath): Unit = {
    //get all bmp name in bmpPath
    val fileList = Paths.get(scalaPath).toFile.listFiles().map(_.getPath.replace(scalaPath, "").dropRight(15)).sortWith(_.toInt < _.toInt)
    fileList.foreach(verifyCAndScala(_, scalaPath, cPath))
  }

  def evalPyScore(pyName: String = "Measure.py", retName: String = "scala_score_bicubic.txt",
                  path: String = scalaTestPath,
                  logger: String = "Evaluate score by using Python", isWrite: Boolean = false): Unit = {
    if(isWrite) {
      val ret = getCmdResult("python3 " + pyName, path, logger)
      val writeScore = new FileWriter(Paths.get(path, retName).toFile)
      val newRet = ret.split("\n").drop(5).mkString("\n")
      writeScore.write(newRet)
      writeScore.close()
    }else{
      doCmd("python3 " + pyName, path, logger)
    }
  }

  def getDoubleAfter(matchStr: String, dest: String) = {
    val doubleFind = "-?(\\d+\\.?\\d+)".r
    try {
      doubleFind.findFirstIn(s"${matchStr}-?(\\d+.?\\d+)".r.findFirstIn(dest).get).get.toDouble
    } catch {
      case e: Exception => -10000.0
    }
  }

  def getStrBeforePSNR(dest: String) = {
    val strFind = "\\w+\\s+\\w+\\.\\w+\\s+\\w+\\s+\\w+\\.\\w+:\\s+".r
    try {
      strFind.findFirstIn(s"\\w+\\s+\\w+\\.\\w+\\s+\\w+\\s+\\w+\\.\\w+:\\s+psnr".r.findFirstIn(dest).get).get
    } catch {
      case e: Exception => "None"
    }
  }

  def showScoreDiff(dutPath: String = scalaTestPath + "scala_score_bicubic.txt",
                    basePath: String = cTestPath + "c_score.txt",
                    diffPath: String = bootPath + "score_diff_bicubic.txt") = {

    val psnr = "psnr\\s+=\\s+"
    val ssim = "ssim\\s+=\\s+"
    val lpips = "lpips\\s+=\\s+"
    val time = "time\\s+=\\s+"

    val baseScore = Source.fromFile(Paths.get(basePath).toFile).getLines().toSeq
    val dutScore = Source.fromFile(Paths.get(dutPath).toFile).getLines().toSeq

    val writeDiff = new FileWriter(Paths.get(diffPath).toFile)

    val cmpCount = Seq(baseScore.length, dutScore.length).min
    (0 until cmpCount).foreach{ i =>
      val baseStrBeforePSNR = getStrBeforePSNR(baseScore(i))
      val dutStrBeforePSNR = getStrBeforePSNR(dutScore(i))
      var valid  = false
      var srcBmp = ""
      var dutBmp  = ""
      if(i != cmpCount - 1) {
        srcBmp = ".{2,3}upsampling.*\\.bmp:".r.findFirstIn(baseStrBeforePSNR).get.dropRight(1)
        dutBmp = ".{2,3}upsampling.*\\.bmp:".r.findFirstIn(dutStrBeforePSNR).get.dropRight(1)
      }
      if(baseStrBeforePSNR.toSeq.take(13) == dutStrBeforePSNR.toSeq.take(13)){
        val basePsnrValue = getDoubleAfter(psnr, baseScore(i))
        val dutPsnrValue = getDoubleAfter(psnr, dutScore(i))
        val baseSsimValue = getDoubleAfter(ssim, baseScore(i))
        val dutSsimValue = getDoubleAfter(ssim, dutScore(i))
        val baseLpipsValue = getDoubleAfter(lpips, baseScore(i))
        val dutLpipsValue = getDoubleAfter(lpips, dutScore(i))
        val baseTimeValue = getDoubleAfter(time, baseScore(i))
        val dutTimeValue = getDoubleAfter(time, dutScore(i))

        var printString = ""
        if(baseStrBeforePSNR.contains("compare")){
          printString = s"The score difference of ${srcBmp} and ${dutBmp} is :\t" + "psnr = " + f"${dutPsnrValue - basePsnrValue}%.3f\t" +
                        "ssim = " + f"${dutSsimValue - baseSsimValue}%.3f\t" + "lpips = " + f"${dutLpipsValue - baseLpipsValue}%.3f\t" +
                        "time = " + f"${dutTimeValue - baseTimeValue}%.3f"
        }
        else{
          printString = s"The score difference of final result is :\t" + "psnr = " + f"${dutPsnrValue - basePsnrValue}%.3f\t" +
                        "ssim = " + f"${dutSsimValue - baseSsimValue}%.3f\t" + "lpips = " + f"${dutLpipsValue - baseLpipsValue}%.3f\t" +
                        "time = " + f"${dutTimeValue - baseTimeValue}%.3f"
        }
        writeDiff.write(printString + "\n")
        println(printString)

      }else{
        println(dutStrBeforePSNR)
        println(s"The ${i}th line of both scoreFiles are not match !")
      }
    }
    writeDiff.close()
  }
}

object TestInterpolation extends App{
//  (0 to 46).foreach { i =>
//    Interpolation.BiQuadratic(3840L, 2160L, fileName = s"${i}.bmp").bmpWrite(s"${i}_upsampling.bmp")
//  }
//  Interpolation.evalPyScore(retName = "c_score.txt", path = cTestPath, isWrite = true)
//  Interpolation.evalPyScore(isWrite = true)
//  Interpolation.verifyAllCAndScala()
//  //Interpolation.getCResult()
//  Interpolation.showScoreDiff(basePath = bootPath + "base_score.txt")
  (0 to 46).foreach { j =>
    Interpolation.BicubicInterpolation(3840L, 2160L, fileName = s"${j}.bmp").bmpWrite(s"${j}_upsampling_bicubic.bmp")
  }
  Interpolation.evalPyScore(isWrite = true)
  Interpolation.showScoreDiff(basePath = bootPath + "base_score_biquadratic.txt")
}