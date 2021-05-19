package Chainsaw

import Chainsaw.FloPoCo.BlackBoxed.SCMWrapper
import Chainsaw.FloPoCo.Transplanted.SCM
import spinal.core.{Component, in, out}
import xilinx.VivadoFlow

import java.nio.file.Paths
import scala.io.Source
import java.io._

object Experiments {

  def main(args: Array[String]): Unit = {
    (0 until 100).map(_ => DSPRand.nextInt(1 << 18) + 4).foreach { constant =>
      testMine(constant, (1 << 14) - 1) // max interval leads to 15 bits
      testMine(constant, (1 << 13) + 1) // min interval leads to 15 bits
      testTheirs(constant) // flopoco design at 15 bits
    }
  }

  def writeFile(fileName: String, content: String) = {
    val f = new File(fileName)
    val updatedContent = Source.fromFile(f).getLines().toSeq :+ content
    val pw = new PrintWriter(f)
    pw.write(updatedContent.mkString("\n"))
    pw.close
  }

  val mineFile = "mineAreaReport.txt"
  val theirFile = "theirAreaReport.txt"

  def testMine(constant: Int, range: Int) = {
    try {
      val mine = VivadoFlow(
        new Component {
          val input = in(UIntReal((1 << 14) - 1))
          val sag = new SCM(input, constant)
          val output = out(sag.implicitValue)
        },
        "SCMMine",
        "synthWorkspace/SCMMine",
        force = true
      ).doit()
      writeFile(mineFile, s"constant=$constant LUT=${mine.LUT}, FF=${mine.FF}")
    }
    catch {
      case _ => writeFile(mineFile, s"failed at constant = $constant")
    }
  }

  def testTheirs(constant: Int) = {
    try {
      val flo = VivadoFlow(
        new SCMWrapper(15, constant),
        "SCMFlo",
        "synthWorkspace/SCMFlo",
        force = true
      ).doit()
      writeFile(theirFile, s"constant=$constant LUT=${flo.LUT}, FF=${flo.FF}")
    }
    catch {
      case _ => writeFile(theirFile, s"failed at constant = $constant")
    }
  }
}
