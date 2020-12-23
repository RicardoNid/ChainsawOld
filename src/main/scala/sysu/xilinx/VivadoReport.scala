package sysu.xilinx

import java.nio.file.Paths

import spinal.core._

import scala.io.Source

class VivadoReport(workspacePath: String,
                   deviceFamily: DeviceFamily,
                   frequencyTarget: HertzNumber = null
                  ) {
  val report = Source.fromFile(Paths.get(workspacePath, "doit.log").toFile).getLines.mkString

  val intFind = "(\\d+,?)+".r // Regex to find int
  val doubleFind = "-?(\\d+\\.?)+".r

  def getIntAfter(pattern: String) = {
    try {
      intFind.findFirstIn(s"${pattern}[ ]*\\|[ ]*(\\d+,?)+".r.findFirstIn(report).get).get.toInt
    }
    catch {
      case e: Exception => -1
    }
  }

  def getDoubleBefore(pattern: String) = {
    try {
      doubleFind.findFirstIn(s"-?(\\d+.?)+ns  \\(${pattern}\\)".r.findFirstIn(report).get).get.toDouble
    }
    catch {
      case e: Exception => -10000.0
    }
  }

  // todo : 实现7-series的报告提取
  /* continue
  extract more attribute from doit.log
  1.build your "XXXFind" regex according to the attribute type
  2.build a "getXXX" method to extract your pattern safely
  3.create a field of VivadoReport, implement the corresponding "printXXX" method
   */

  val LUT = if (deviceFamily == UltraScale) getIntAfter("CLB LUTs\\*") else 0
  val FF = if (deviceFamily == UltraScale) getIntAfter("CLB Registers") else 0
  val DSP = if (deviceFamily == UltraScale) getIntAfter("DSPs") else 0
  val BRAM = if (deviceFamily == UltraScale) getIntAfter("Block RAM Tile") else 0

  val targetPeriod = (if (frequencyTarget != null) frequencyTarget else 400 MHz).toTime
  val slack = getDoubleBefore("required time - arrival time")
  val Frequency = 1.0 / (targetPeriod.toDouble - slack * 1e-9)


  def printArea = println(s"LUT: ${LUT}\nFF: ${FF}\nDSP: ${DSP}\nBRAM: ${BRAM}\n")

  def printFMax = println(s"Frequency: ${Frequency / 1E6} MHz\n")

  // fixme : not working now
  def getReport = Array(LUT.toString, FF.toString, DSP.toString, BRAM.toString, Frequency.toString)
}
