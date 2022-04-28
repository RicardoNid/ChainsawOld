package Chainsaw.algos

import scala.sys.process._
import java.io._
import java.nio.file.Paths
import scala.io._
import PathInfo._
import breeze.linalg._

import scala.math._
import breeze.math._

import scala.collection.mutable.ArrayBuffer

object Interpolation {

//  def getPixels(image: BMPImage, x: Int, y: Int) = {
//    case class retPixel(var r: Short, var g: Short, var b: Short) {}
//    val ret        = retPixel(0, 0, 0)
//    var xIdx, yIdx = 0
//    if (x > image.header.height_px - 1) { xIdx = image.header.height_px - 1 }
//    else if (x < 0) { xIdx = 0 }
//    else { xIdx = x }
//    if (y > image.header.width_px - 1) { yIdx = image.header.width_px - 1 }
//    else if (y < 0) { yIdx = 0 }
//    else { yIdx = y }
//    image.height >= 0 match {
//      case true =>
//        ret.r = image.pixels.r(yIdx + image.header.width_px * (image.header.height_px - xIdx - 1))
//        ret.g = image.pixels.g(yIdx + image.header.width_px * (image.header.height_px - xIdx - 1))
//        ret.b = image.pixels.b(yIdx + image.header.width_px * (image.header.height_px - xIdx - 1))
//      case false =>
//        ret.r = image.pixels.r(yIdx + xIdx * image.header.width_px)
//        ret.g = image.pixels.g(yIdx + xIdx * image.header.width_px)
//        ret.b = image.pixels.b(yIdx + xIdx * image.header.width_px)
//    }
//    ret
//  }

  def getBiquadFactor(srcBmp: BMPImage, tagBmp: BMPImage, xDPos: Double, xIPos: Int, yDPos: Double, yIPos: Int) = {
    val xDiff                                  = xDPos - xIPos
    val yDiff                                  = yDPos - yIPos
    val coffs                                  = Array.ofDim[Int](2, 2)
    var xFactor0, xFactor1, yFactor0, yFactor1 = 0.toShort
    xFactor0 = ((1.toDouble - xDiff) * 4).toShort
    xFactor1 = (4 - xFactor0).toShort
    yFactor0 = ((1.toDouble - yDiff) * 4).toShort
    yFactor1 = (4 - yFactor0).toShort

    coffs(0)(0) = xFactor0 * yFactor0
    coffs(0)(1) = xFactor0 * yFactor1
    coffs(1)(0) = xFactor1 * yFactor0
    coffs(1)(1) = xFactor1 * yFactor1

    coffs
  }

  def BiQuadratic(
      tagWidth: Int,
      tagHeight: Int,
      srcWidth: Int  = 960,
      srcHeight: Int = 540,
      fileName: String,
      filePath: String = scalaDownPath
  ): BMPImage = {
    // load source bmp image
    val srcBmp = BMPImage(srcWidth, srcHeight)
    srcBmp.bmpRead(fileName, filePath)
    // generate target bmp image class
    val tagBmp = BMPImage(tagWidth, tagHeight)

    // do interpolation
    val yDoubleScale = srcBmp.header.width_px.toDouble / tagBmp.header.width_px.toDouble
    val xDoubleScale = srcBmp.header.height_px.toDouble / tagBmp.header.height_px.toDouble

    (0 until tagBmp.header.height_px).foreach { x =>
      val xDPosition = x * xDoubleScale + 0.5 * (xDoubleScale - 1)
      var xIPosition = floor(xDPosition).toInt
      (0 until tagBmp.header.width_px).foreach { y =>
        val yDPosition = y * yDoubleScale + 0.5 * (yDoubleScale - 1)
        var yIPosition = floor(yDPosition).toInt
        val coffs      = getBiquadFactor(srcBmp, tagBmp, xDPosition, xIPosition, yDPosition, yIPosition)

        if (xIPosition < 0) { xIPosition = 0 }
        else if (xIPosition > srcBmp.header.height_px - 1) { xIPosition = srcBmp.header.height_px - 1 }
        if (yIPosition < 0) { yIPosition = 0 }
        else if (yIPosition > srcBmp.header.width_px - 1) { yIPosition = srcBmp.header.width_px - 1 }

        Range(0, 3).foreach { i =>
          tagBmp.assignPixels(
            x,
            y,
            ((coffs(0)(0) * srcBmp.getPixels(xIPosition, yIPosition)(i)
              + coffs(0)(1) * srcBmp.getPixels(xIPosition, yIPosition + 1)(i)
              + coffs(1)(0) * srcBmp.getPixels(xIPosition + 1, yIPosition)(i)
              + coffs(1)(1) * srcBmp.getPixels(xIPosition + 1, yIPosition + 1)(i)) / 16).toShort,
            i
          )
        }
      }

    }
    tagBmp
  }

  def getCovFactor(x: Double, a: Double = -0.5) = {
    var ret = 0d
    if (x.abs >= 0 && x.abs <= 1) {
      ret = 1 - (a + 3) * pow(x, 2) + (a + 2) * pow(x.abs, 3)
    } else if (x.abs > 1 && x.abs <= 2) {
      ret = -4 * a + 8 * a * x.abs - 5 * a * pow(x, 2) + a * pow(x.abs, 3)
    } else {
      ret = 0d
    }
    ret
  }

  def BicubicInterpolation(
      tagWidth: Int,
      tagHeight: Int,
      srcWidth: Int  = 960,
      srcHeight: Int = 540,
      a: Double      = -0.5,
      fileName: String,
      filePath: String = scalaDownPath
  ): BMPImage = {
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
      //if(xDPosition < 0) {xDPosition = 0}
      var xIPosition = floor(xDPosition).toInt
      val xDiff      = xDPosition - xIPosition
      if (xIPosition < 0) { xIPosition = 0 }
      else if (xIPosition > srcBmp.header.height_px - 1) { xIPosition = srcBmp.header.height_px - 1 }
      (0 until tagBmp.header.width_px).foreach { y =>
        var yDPosition = y * yDoubleScale + 0.5 * (yDoubleScale - 1)
        //if(xDPosition < 0) {yDPosition = 0}
        var yIPosition = floor(yDPosition).toInt
        val yDiff      = yDPosition - yIPosition
        if (yIPosition < 0) { yIPosition = 0 }
        else if (yIPosition > srcBmp.header.width_px - 1) { yIPosition = srcBmp.header.width_px - 1 }

        val rowFactorVector = DenseVector(
          (getCovFactor(1 + xDiff, a) * 16).toInt,
          (getCovFactor(xDiff, a) * 16).toInt,
          (getCovFactor(1 - xDiff, a) * 16).toInt,
          (getCovFactor(2 - xDiff, a) * 16).toInt
        ).t
        val colFactorVector = DenseVector(
          (getCovFactor(1 + yDiff, a) * 16).toInt,
          (getCovFactor(yDiff, a) * 16).toInt,
          (getCovFactor(1 - yDiff, a) * 16).toInt,
          (getCovFactor(2 - yDiff, a) * 16).toInt
        )

        val PixelsMatrixs = new Array[DenseMatrix[Int]](3)

        Range(0, 3).foreach { i =>
          PixelsMatrixs(i) = DenseMatrix(
            (
              srcBmp.getPixels(xIPosition - 1, yIPosition - 1)(i),
              srcBmp.getPixels(xIPosition - 1, yIPosition)(i),
              srcBmp.getPixels(xIPosition - 1, yIPosition + 1)(i),
              srcBmp.getPixels(xIPosition - 1, yIPosition + 2)(i)
            ),
            (
              srcBmp.getPixels(xIPosition, yIPosition - 1)(i),
              srcBmp.getPixels(xIPosition, yIPosition)(i),
              srcBmp.getPixels(xIPosition, yIPosition + 1)(i),
              srcBmp.getPixels(xIPosition, yIPosition + 2)(i)
            ),
            (
              srcBmp.getPixels(xIPosition + 1, yIPosition - 1)(i),
              srcBmp.getPixels(xIPosition + 1, yIPosition)(i),
              srcBmp.getPixels(xIPosition + 1, yIPosition + 1)(i),
              srcBmp.getPixels(xIPosition + 1, yIPosition + 2)(i)
            ),
            (
              srcBmp.getPixels(xIPosition + 2, yIPosition - 1)(i),
              srcBmp.getPixels(xIPosition + 2, yIPosition)(i),
              srcBmp.getPixels(xIPosition + 2, yIPosition + 1)(i),
              srcBmp.getPixels(xIPosition + 2, yIPosition + 2)(i)
            )
          ).map(_.toInt)
        }
        Range(0, 3).foreach { i =>
          tagBmp.assignPixels(x, y, (rowFactorVector * PixelsMatrixs(i) * colFactorVector / 256).abs.toShort, i)
        }
      }
    }
    tagBmp
  }

  def FastNediInterpolation(
      threshold: Int,
      tagWidth: Int,
      tagHeight: Int,
      srcWidth: Int  = 960,
      srcHeight: Int = 540,
      fileName: String,
      filePath: String = scalaDownPath
  ) = {
    // load source bmp image
    val srcBmp = BMPImage(srcWidth, srcHeight)
    srcBmp.bmpRead(fileName, filePath)
    // generate target bmp image class
    val tagBmp = BMPImage(tagWidth, tagHeight)

    Range(0, tagBmp.header.height_px).foreach { x =>
      Range(0, tagBmp.header.width_px).foreach { y =>
        x % 4 match {
          case 0 =>
            y % 4 match {
              case 0 =>
                Range(0, 3).foreach { i =>
                  tagBmp.assignPixels(x, y, srcBmp.getPixels(x / 4, y / 4)(i), i)
                }
              case 2 =>
                Range(0, 3).foreach { k =>
                  tagBmp.assignPixels(x, y, ((srcBmp.getPixels(x / 4, (y - 2) / 4)(k) + srcBmp.getPixels(x / 4, (y + 2) / 4)(k)) / 2).toShort, k)
                }
              case _ =>
            }
          case 2 =>
            y % 4 match {
              case 0 =>
                Range(0, 3).foreach { i =>
                  tagBmp.assignPixels(x, y, ((srcBmp.getPixels((x - 2) / 4, y / 4)(i) + srcBmp.getPixels((x + 2) / 4, y / 4)(i)) / 2).toShort, i)
                }
              case 2 =>
                Range(0, 3).foreach { k =>
                  tagBmp.assignPixels(
                    x,
                    y,
                    ((srcBmp.getPixels((x - 2) / 4, (y - 2) / 4)(k) + srcBmp.getPixels((x + 2) / 4, (y + 2) / 4)(k) + srcBmp
                      .getPixels((x - 2) / 4, (y + 2) / 4)(k) + srcBmp.getPixels((x + 2) / 4, (y - 2) / 4)(k)) / 4).toShort,
                    k
                  )
                }
              case _ =>
            }
          case _ =>
        }
      }
    }

    Range(0, tagBmp.header.height_px).foreach { x =>
      Range(0, tagBmp.header.width_px).foreach { y =>
        x % 2 match {
          case 0 =>
            y % 2 match {
              case 1 =>
                Range(0, 3).foreach { k =>
                  if ((srcBmp.getPixels(x / 4, (y - 1) / 4)(k) - srcBmp.getPixels(x / 4, (y + 1) / 4)(k)).abs >= threshold) {
                    tagBmp.assignPixels(x, y, 0.toShort, k)
                  } else {
                    tagBmp.assignPixels(x, y, ((srcBmp.getPixels(x / 4, (y - 1) / 4)(k) + srcBmp.getPixels(x / 4, (y + 1) / 4)(k)) / 2).toShort, k)
                  }
                }
              case _ =>
            }

          case 1 =>
            y % 2 match {
              case 0 =>
                Range(0, 3).foreach { k =>
                  if ((srcBmp.getPixels((x - 1) / 4, y / 4)(k) - srcBmp.getPixels((x + 1) / 4, y / 4)(k)).abs >= threshold) {
                    tagBmp.assignPixels(x, y, 0.toShort, k)
                  } else {
                    tagBmp.assignPixels(x, y, ((srcBmp.getPixels((x - 1) / 4, y / 4)(k) + srcBmp.getPixels((x + 1) / 4, y / 4)(k)) / 2).toShort, k)
                  }
                }
              case 1 =>
                Range(0, 3).foreach { k =>
                  val diag1 = (srcBmp.getPixels((x - 1) / 4, (y - 1) / 4)(k) - srcBmp.getPixels((x + 1) / 4, (y + 1) / 4)(k)).abs
                  val diag2 = (srcBmp.getPixels((x + 1) / 4, (y - 1) / 4)(k) - srcBmp.getPixels((x - 1) / 4, (y + 1) / 4)(k)).abs
                  val diff  = Seq(diag1, diag2).min
                  if (diff >= threshold) {
                    tagBmp.assignPixels(x, y, 0.toShort, k)
                  } else {
                    if (diag1 >= diag2) {
                      tagBmp.assignPixels(
                        x,
                        y,
                        ((srcBmp.getPixels((x + 1) / 4, (y - 1) / 4)(k) + srcBmp.getPixels((x - 1) / 4, (y + 1) / 4)(k)) / 2).toShort,
                        k
                      )
                    } else {
                      tagBmp.assignPixels(
                        x,
                        y,
                        ((srcBmp.getPixels((x - 1) / 4, (y - 1) / 4)(k) + srcBmp.getPixels((x + 1) / 4, (y + 1) / 4)(k)) / 2).toShort,
                        k
                      )
                    }
                  }
                }
              case _ =>
            }
          case _ =>
        }
      }
    }


    Range(0, tagBmp.header.height_px).foreach { x =>
      Range(0, tagBmp.header.width_px).foreach { y =>
        if (tagBmp.getPixels(x, y) == 0) {
          val candidates = new ArrayBuffer[(Short, Short)]()
          Range(0, 3).foreach { i =>
            if (tagBmp.getPixels(x, y - 1)(i) != 0 && tagBmp.getPixels(x, y + 1)(i) != 0) {
              candidates += Tuple2(
                (tagBmp.getPixels(x, y - 1)(i) - tagBmp.getPixels(x, y + 1)(i)).abs.toShort,
                (tagBmp.getPixels(x, y - 1)(i) + tagBmp.getPixels(x, y + 1)(i) / 2).toShort
              )
            }

            if (tagBmp.getPixels(x - 1, y)(i) != 0 && tagBmp.getPixels(x + 1, y)(i) != 0) {
              candidates += Tuple2(
                (tagBmp.getPixels(x - 1, y)(i) - tagBmp.getPixels(x + 1, y)(i)).abs.toShort,
                (tagBmp
                  .getPixels(x - 1, y)(i) + tagBmp.getPixels(x + 1, y)(i) / 2).toShort
              )
            }

            if (tagBmp.getPixels(x - 1, y - 1)(i) != 0 && tagBmp.getPixels(x + 1, y + 1)(i) != 0) {
              candidates += Tuple2(
                (tagBmp.getPixels(x - 1, y - 1)(i) - tagBmp.getPixels(x + 1, y + 1)(i)).abs.toShort,
                (tagBmp
                  .getPixels(x - 1, y - 1)(i) + tagBmp.getPixels(x + 1, y + 1)(i) / 2).toShort
              )
            }

            if (tagBmp.getPixels(x + 1, y - 1)(i) != 0 && tagBmp.getPixels(x - 1, y + 1)(i) != 0) {
              candidates += Tuple2(
                (tagBmp.getPixels(x + 1, y - 1)(i) - tagBmp.getPixels(x - 1, y + 1)(i)).abs.toShort,
                (tagBmp
                  .getPixels(x + 1, y - 1)(i) + tagBmp.getPixels(x - 1, y + 1)(i) / 2).toShort
              )
            }
            val sortCandidate = candidates.sortBy(_._1)
            tagBmp.assignPixels(x, y, sortCandidate(0)._2, i)
          }
        }
      }
    }

//    // step1: interpolate the homogenous pixels
//    Range(0, tagBmp.header.height_px).foreach { x =>
//      Range(0, tagBmp.header.width_px).foreach { y =>
//        x % 4 match {
//          case 0 =>
//            y % 4 match {
//              case 0 =>
//                Range(0, 3).foreach { i =>
//                  tagBmp.assignPixels(x, y, srcBmp.getPixels(x / 4, y / 4)(i), i)
//                }
//              case 1 =>
//                Range(0, 3).foreach { j =>
//                  tagBmp.assignPixels(x, y, (3 * (srcBmp.getPixels(x / 4, (y - 1) / 4)(j) + srcBmp.getPixels(x / 4, (y + 3) / 4)(j)) / 4).toShort, j)
//                }
//              case 2 =>
//                Range(0, 3).foreach { k =>
//                  if ((srcBmp.getPixels(x / 4, (y - 2) / 4)(k) - srcBmp.getPixels(x / 4, (y + 2) / 4)(k)).abs >= threshold) {
//                    tagBmp.assignPixels(x, y, 0.toShort, k)
//                  } else {
//                    tagBmp.assignPixels(x, y, ((srcBmp.getPixels(x / 4, (y - 2) / 4)(k) + srcBmp.getPixels(x / 4, (y + 2) / 4)(k)) / 2).toShort, k)
//                  }
//                }
//              case 3 =>
//                Range(0, 3).foreach { l =>
//                  tagBmp.assignPixels(x, y, ((srcBmp.getPixels(x / 4, (y - 3) / 4)(l) + 3 * srcBmp.getPixels(x / 4, (y + 1) / 4)(l)) / 4).toShort, l)
//                }
//            }
//
//          case 1 =>
//            y % 4 match {
//              case 0 =>
//                Range(0, 3).foreach { i =>
//                  tagBmp.assignPixels(x, y, (3 * (srcBmp.getPixels((x - 1) / 4, y / 4)(i) + srcBmp.getPixels((x + 3) / 4, y)(i)) / 4).toShort, i)
//                }
//              case 1 =>
//                Range(0, 3).foreach { j =>
//                  tagBmp.assignPixels(
//                    x,
//                    y,
//                    ((3 * srcBmp.getPixels((x - 1) / 4, (y - 1) / 4)(j) + srcBmp.getPixels((x + 3) / 4, (y + 3) / 4)(j)) / 4).toShort,
//                    j
//                  )
//                }
//              case 2 =>
//                Range(0, 3).foreach { k =>
//                  tagBmp.assignPixels(x, y, 0.toShort, k)
//                }
//              case 3 =>
//                Range(0, 3).foreach { l =>
//                  tagBmp.assignPixels(
//                    x,
//                    y,
//                    ((3 * srcBmp.getPixels((x - 1) / 4, (y + 1) / 4)(l) + srcBmp.getPixels((x + 3) / 4, (y - 3) / 4)(l)) / 4).toShort,
//                    l
//                  )
//                }
//
//            }
//          case 2 =>
//            y % 4 match {
//              case 0 =>
//                Range(0, 3).foreach { i =>
//                  if ((srcBmp.getPixels((x - 2) / 4, y / 4)(i) - srcBmp.getPixels((x + 2) / 4, y / 4)(i)).abs >= threshold) {
//                    tagBmp.assignPixels(x, y, 0.toShort, i)
//                  } else {
//                    tagBmp.assignPixels(x, y, ((srcBmp.getPixels((x - 2) / 4, y / 4)(i) + srcBmp.getPixels((x + 2) / 4, y / 4)(i)) / 2).toShort, i)
//                  }
//                }
//              case 1 =>
//                Range(0, 3).foreach { j =>
//                  tagBmp.assignPixels(x, y, 0.toShort, j)
//                }
//              case 2 =>
//                Range(0, 3).foreach { k =>
//                  val diag1 = (srcBmp.getPixels((x - 2) / 4, (y - 2) / 4)(k) - srcBmp.getPixels((x + 2) / 4, (y + 2) / 4)(k)).abs
//                  val diag2 = (srcBmp.getPixels((x + 2) / 4, (y - 2) / 4)(k) - srcBmp.getPixels((x - 2) / 4, (y + 2) / 4)(k)).abs
//                  val diff  = Seq(diag1, diag2).min
//                  if (diff >= threshold) {
//                    tagBmp.assignPixels(x, y, 0.toShort, k)
//                  } else {
//                    if (diag1 >= diag2) {
//                      tagBmp.assignPixels(
//                        x,
//                        y,
//                        ((srcBmp.getPixels((x + 2) / 4, (y - 2) / 4)(k) + srcBmp.getPixels((x - 2) / 4, (y + 2) / 4)(k)) / 2).toShort,
//                        k
//                      )
//                    } else {
//                      tagBmp.assignPixels(
//                        x,
//                        y,
//                        ((srcBmp.getPixels((x - 2) / 4, (y - 2) / 4)(k) + srcBmp.getPixels((x + 2) / 4, (y + 2) / 4)(k)) / 2).toShort,
//                        k
//                      )
//                    }
//                  }
//                }
//              case 3 =>
//                Range(0, 3).foreach { l =>
//                  tagBmp.assignPixels(x, y, 0.toShort, l)
//                }
//            }
//          case 3 =>
//            y % 4 match {
//              case 0 =>
//                Range(0, 3).foreach { i =>
//                  tagBmp.assignPixels(x, y, ((srcBmp.getPixels((x - 3) / 4, y / 4)(i) + 3 * srcBmp.getPixels((x + 1) / 4, y)(i)) / 4).toShort, i)
//                }
//              case 1 =>
//                Range(0, 3).foreach { j =>
//                  tagBmp.assignPixels(
//                    x,
//                    y,
//                    ((3 * srcBmp.getPixels((x + 1) / 4, (y - 1) / 4)(j) + srcBmp.getPixels((x - 3) / 4, (y + 3) / 4)(j)) / 4).toShort,
//                    j
//                  )
//                }
//              case 2 =>
//                Range(0, 3).foreach { k =>
//                  tagBmp.assignPixels(x, y, 0.toShort, k)
//                }
//              case 3 =>
//                Range(0, 3).foreach { l =>
//                  tagBmp.assignPixels(
//                    x,
//                    y,
//                    ((3 * srcBmp.getPixels((x + 1) / 4, (y + 1) / 4)(l) + srcBmp.getPixels((x - 3) / 4, (y - 3) / 4)(l)) / 4).toShort,
//                    l
//                  )
//                }
//            }
//          case _ =>
//        }
//      }
//    }
//
//    // step2: interpolate the edge pixels and other special pixels
//    Range(0, tagBmp.header.height_px).foreach { x =>
//      Range(0, tagBmp.header.width_px).foreach { y =>
//        if (tagBmp.getPixels(x, y) == 0) {
//          val candidates = new ArrayBuffer[(Short, Short)]()
//          Range(0, 3).foreach { i =>
//            if (tagBmp.getPixels(x, y - 1)(i) != 0 && tagBmp.getPixels(x, y + 1)(i) != 0) {
//              candidates += Tuple2(
//                (tagBmp.getPixels(x, y - 1)(i) - tagBmp.getPixels(x, y + 1)(i)).abs.toShort,
//                (tagBmp.getPixels(x, y - 1)(i) + tagBmp.getPixels(x, y + 1)(i) / 2).toShort
//              )
//            }
//
//            if (tagBmp.getPixels(x - 1, y)(i) != 0 && tagBmp.getPixels(x + 1, y)(i) != 0) {
//              candidates += Tuple2(
//                (tagBmp.getPixels(x - 1, y)(i) - tagBmp.getPixels(x + 1, y)(i)).abs.toShort,
//                (tagBmp
//                  .getPixels(x - 1, y)(i) + tagBmp.getPixels(x + 1, y)(i) / 2).toShort
//              )
//            }
//
//            if (tagBmp.getPixels(x - 1, y - 1)(i) != 0 && tagBmp.getPixels(x + 1, y + 1)(i) != 0) {
//              candidates += Tuple2(
//                (tagBmp.getPixels(x - 1, y - 1)(i) - tagBmp.getPixels(x + 1, y + 1)(i)).abs.toShort,
//                (tagBmp
//                  .getPixels(x - 1, y - 1)(i) + tagBmp.getPixels(x + 1, y + 1)(i) / 2).toShort
//              )
//            }
//
//            if (tagBmp.getPixels(x + 1, y - 1)(i) != 0 && tagBmp.getPixels(x - 1, y + 1)(i) != 0) {
//              candidates += Tuple2(
//                (tagBmp.getPixels(x + 1, y - 1)(i) - tagBmp.getPixels(x - 1, y + 1)(i)).abs.toShort,
//                (tagBmp
//                  .getPixels(x + 1, y - 1)(i) + tagBmp.getPixels(x - 1, y + 1)(i) / 2).toShort
//              )
//            }
//            val sortCandidate = candidates.sortBy(_._1)
//            tagBmp.assignPixels(x, y, sortCandidate(0)._2, i)
//          }
//        }
//      }
//    }
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
    if (ret == 0) {
      println("Finish upsampling !")
    }
  }

  def getAllCResult(cPath: String = cCodePath, bmpPath: String = cDownPath): Unit = {
    //get all bmp name in bmpPath
    val fileList = Paths.get(bmpPath).toFile.listFiles().map(_.getPath.replace(bmpPath, "").dropRight(4)).sortWith(_.toInt < _.toInt)
    fileList.foreach(getCResult(_, cPath))
  }

  def verifyCAndScala(
      srcBmp: String    = "0",
      scalaPath: String = scalaUpPath,
      cPath: String     = cUpPath,
      logger: String    = "verify C and Scala program function"
  ): Unit = {
    getCResult(srcBmp)
    if (doCmd("cmp " + scalaPath + s"${srcBmp}_upsampling.bmp" + " " + cPath + s"${srcBmp}_upsampling.bmp", scalaPath, logger) == 0)
      println(s"The upsampling result of C and Scala for ${srcBmp} is same !")
  }

  def verifyAllCAndScala(scalaPath: String = scalaUpPath, cPath: String = cUpPath): Unit = {
    //get all bmp name in bmpPath
    val fileList = Paths.get(scalaPath).toFile.listFiles().map(_.getPath.replace(scalaPath, "").dropRight(15)).sortWith(_.toInt < _.toInt)
    fileList.foreach(verifyCAndScala(_, scalaPath, cPath))
  }

  def evalPyScore(
      pyName: String   = "Measure.py",
      path: String     = scalaTestPath,
      srcPath: String  = scalaUpPath,
      retName: String  = "scala_score_fastNedi.txt",
      logger: String   = "Evaluate score by using Python",
      isWrite: Boolean = false
  ): Unit = {
    if (isWrite) {
      val ret        = getCmdResult("python3 " + pyName + " -dirB " + srcPath, path, logger)
      val writeScore = new FileWriter(Paths.get(path, retName).toFile)
      val newRet     = ret.split("\n").drop(5).mkString("\n")
      writeScore.write(newRet)
      writeScore.close()
    } else {
      doCmd("python3 " + pyName + " -dirB " + srcPath, path, logger)
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
    val strFind = "\\w+\\s+\\w+\\.\\w+\\s+\\w+\\s+\\w+(-?\\w?\\.?\\w+)\\.\\w+:\\s+".r
    try {
      strFind.findFirstIn(s"\\w+\\s+\\w+\\.\\w+\\s+\\w+\\s+\\w+(-?\\w?\\.?\\w+)\\.\\w+:\\s+psnr".r.findFirstIn(dest).get).get
    } catch {
      case e: Exception => "None"
    }
  }

  def showScoreDiff(
      dutPath: String  = scalaTestPath + "scala_score_fastNedi.txt",
      basePath: String = cTestPath + "c_score.txt",
      diffPath: String = bootPath + "score_diff_fastNedi.txt"
  ) = {

    val psnr  = "psnr\\s+=\\s+"
    val ssim  = "ssim\\s+=\\s+"
    val lpips = "lpips\\s+=\\s+"
    val time  = "time\\s+=\\s+"

    val baseScore = Source.fromFile(Paths.get(basePath).toFile).getLines().toSeq
    val dutScore  = Source.fromFile(Paths.get(dutPath).toFile).getLines().toSeq
    val writeDiff = new FileWriter(Paths.get(diffPath).toFile)

    val cmpCount = Seq(baseScore.length, dutScore.length).min
    (0 until cmpCount).foreach { i =>
      val baseStrBeforePSNR = getStrBeforePSNR(baseScore(i))
      val dutStrBeforePSNR  = getStrBeforePSNR(dutScore(i))
      var srcBmp            = ""
      var dutBmp            = ""
      if (i != cmpCount - 1) {
        srcBmp = ".{2,3}upsampling.*\\.bmp:".r.findFirstIn(baseStrBeforePSNR).get.dropRight(1)
        dutBmp = ".{2,3}upsampling.*\\.bmp:".r.findFirstIn(dutStrBeforePSNR).get.dropRight(1)
      }
      if (baseStrBeforePSNR.toSeq.take(13) == dutStrBeforePSNR.toSeq.take(13)) {
        val basePsnrValue  = getDoubleAfter(psnr, baseScore(i))
        val dutPsnrValue   = getDoubleAfter(psnr, dutScore(i))
        val baseSsimValue  = getDoubleAfter(ssim, baseScore(i))
        val dutSsimValue   = getDoubleAfter(ssim, dutScore(i))
        val baseLpipsValue = getDoubleAfter(lpips, baseScore(i))
        val dutLpipsValue  = getDoubleAfter(lpips, dutScore(i))
        val baseTimeValue  = getDoubleAfter(time, baseScore(i))
        val dutTimeValue   = getDoubleAfter(time, dutScore(i))

        var printString = ""
        if (baseStrBeforePSNR.contains("compare")) {
          printString = s"The score difference of ${srcBmp} and ${dutBmp} is :\t" + "psnr = " + f"${dutPsnrValue - basePsnrValue}%.3f\t" +
            "ssim = " + f"${dutSsimValue - baseSsimValue}%.3f\t" + "lpips = " + f"${dutLpipsValue - baseLpipsValue}%.3f\t" +
            "time = " + f"${dutTimeValue - baseTimeValue}%.3f"
        } else {
          printString = s"The score difference of final result is :\t" + "psnr = " + f"${dutPsnrValue - basePsnrValue}%.3f\t" +
            "ssim = " + f"${dutSsimValue - baseSsimValue}%.3f\t" + "lpips = " + f"${dutLpipsValue - baseLpipsValue}%.3f\t" +
            "time = " + f"${dutTimeValue - baseTimeValue}%.3f"
        }
        writeDiff.write(printString + "\n")
        println(printString)

      } else {
        println(s"The ${i}th line of both scoreFiles are not match !")
      }
    }
    writeDiff.close()
  }
}

object TestInterpolation extends App {
//  (0 to 46).foreach { i =>
//    println(s"Interpolate the ${i}th image !")
//    Interpolation.BiQuadratic(3840, 2160, fileName = s"${i}.bmp").bmpWrite(s"${i}_upsampling_biquadratic.bmp", scalaTestPath + "biquadratic_upsampling/")
//  }
//  println("Begin evaluate the score !")
//  Interpolation.evalPyScore(path = scalaTestPath, srcPath = scalaTestPath + "biquadratic_upsampling/", retName = "scala_score_biquadratic.txt", isWrite = true)
//  println("Begin get the score difference !")
//  Interpolation.showScoreDiff(dutPath = scalaTestPath + "scala_score_biquadratic.txt", basePath = bootPath + "base_score_biquadratic.txt", diffPath = bootPath + "score_diff_biquadratic.txt")

  // Test bicubic
//  Interpolation.doCmd(s"rm -rf ${scalaUpPath}", scalaTestPath, "remove old bmp!")
//  Interpolation.doCmd(s"mkdir ${scalaUpPath}", scalaTestPath, "new a dict!")
//  val i = -0.1
//  (0 to 46).foreach { j =>
//    println(s"Interpolate the ${j}th image using a : ${i} !")
//    Interpolation.BicubicInterpolation(3840, 2160, fileName = s"${j}.bmp", a = i).bmpWrite(s"${j}_upsampling_bicubic_${i}.bmp")
//  }
//  println("Begin evaluate the score !")
//  Interpolation.evalPyScore(retName = s"scala_score_bicubic_${i}.txt", isWrite = true)
//  println("Begin get the score difference !")
//  Interpolation.showScoreDiff(dutPath = scalaTestPath + s"scala_score_bicubic_${i}.txt", basePath = bootPath + "base_score_biquadratic.txt")

  Range(112, 193, 4).foreach { i =>
    Interpolation.doCmd(s"rm -rf ${scalaTestPath}fastNedi_upsampling_${i}", scalaTestPath, "remove old bmp!")
    Interpolation.doCmd(s"mkdir ${scalaTestPath}fastNedi_upsampling_${i}", scalaTestPath, "new a dict!")
    (0 to 46).foreach { j =>
      println(s"Interpolate the ${j}th image using threshold is ${i} !")
      Interpolation
        .FastNediInterpolation(i, 3840, 2160, fileName = s"${j}.bmp")
        .bmpWrite(s"${j}_upsampling_fastNedi.bmp", filePath = s"${scalaTestPath}fastNedi_upsampling_${i}")
    }
    println(s"Begin evaluate the score for threshold ${i}!")
    Interpolation.evalPyScore(srcPath = s"${scalaTestPath}fastNedi_upsampling_${i}", retName = s"scala_score_fastNedi_${i}.txt", isWrite = true)
    println(s"Begin get the score difference for threshold ${i} !")
    Interpolation.showScoreDiff(
      dutPath  = scalaTestPath + s"scala_score_fastNedi_${i}.txt",
      basePath = bootPath + "base_score_biquadratic.txt",
      diffPath = bootPath + s"score_diff_fastNedi_${i}"
    )
  }

}
