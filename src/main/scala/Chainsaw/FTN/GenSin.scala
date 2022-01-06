package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

import java.io.File
import org.apache.commons.io.FileUtils

object GenSin extends App {

  val scaling = 0.15
  val data = Seq.fill(4096)((0 until 128).map(_ * scaling).map(_.toInt))
  val dataString = data.map(_.map(BigInt(_).toString(16).padToLeft(2, '0')).mkString(" ")).mkString("\n")

  val file = new File("sin.txt")
  FileUtils.write(file, dataString)
}
