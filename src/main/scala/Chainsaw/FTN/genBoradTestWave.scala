package Chainsaw.FTN

import Chainsaw._
import breeze.numerics._
import breeze.numerics.constants._
import org.apache.commons.io.FileUtils

import java.io.File

object genBoradTestWave extends App {

  def genFile(filename: String, fileData: Seq[Seq[Int]]) = {
    require(fileData.flatten.forall(value => value >= 0 && value < 64))
    val dataString = fileData.map(_.map(BigInt(_).toString(16).padToLeft(2, '0')).mkString(" ")).mkString("\n")
    val file = new File(s"./testWave/$filename")
    FileUtils.write(file, dataString)
  }

  def genTri(period: Int, scaling: Double, offset: Int) = {
    val num = 4096 * 128 / period
    val scalingInUse = 64.0 * scaling / period
    val data = Seq.fill(num)((0 until period).map(_ * scalingInUse).map(_.toInt + offset)) // tri data
    val filename = s"tri_period_${period}_scale_${scaling}_offset_${offset}.txt"
    genFile(filename, data)
  }

  def genSin(period: Int, scaling: Double) = {
    val num = 4096 * 128 / period
    val step = 2 * Pi / period
    val scalingInUse = 32.0 * scaling
    val data = Seq.fill(num)((0 until period).map(i => sin(i * step) * scalingInUse + 32).map(_.toInt)) // sin data
    val filename = s"sin_${period}_scale_${scaling}.txt"
    genFile(filename, data)
  }

  def genRandomWithPreamble() = {
    val data = Seq.fill(1)((0 until 64).map(_ => ChainsawRand.nextInt(64) * 0.3).map(_.toInt) ++ Seq.fill(64)(0)) // sin data
    val filename = s"randomWithZero.txt"
    genFile(filename, data)
  }

  Seq(2, 4, 8, 16, 32, 64, 128).foreach(genTri(_, 1 / 0.7 * 0.5, 0))
  Seq(2, 4, 8, 16, 32, 64, 128).foreach(genSin(_, 1 / 0.7 * 0.5))
  //  Seq(2, 4, 8, 16, 32, 64, 128).foreach(genSin(_, 0.32))
  //  Seq(2, 4, 8, 16, 32, 64, 128).foreach(genSin(_, 0.3))

  genRandomWithPreamble()

}
