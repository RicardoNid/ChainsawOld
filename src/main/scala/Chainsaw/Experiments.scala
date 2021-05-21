package Chainsaw

import Chainsaw.FloPoCo.BlackBoxed.SCMWrapper
import Chainsaw.FloPoCo.Transplanted.SCM
import spinal.core.{Component, in, out, _}
import xilinx.VivadoFlow

import java.io._
import scala.io.Source

object Experiments {

  def main(args: Array[String]): Unit = {
    //    testAddtion
    //    testMultiplicationSignedness
    //    Seq(179106)
    //    Seq(190730) this is a testCase of with only additions

    //    SpinalConfig().generateVhdl(new Component {
    //      val input = in(UIntReal((1 << 14) - 1))
    //      val sag = new SCM(input, 190730)
    //      val output = out(sag.implicitValue)
    //    })


    //    GenRTL(new FloPoCo.BlackBoxed.SCMWrapper(15, 44))
    testSCMs
  }

  private def testSCMs = {
    (0 until 100).map(_ => DSPRand.nextInt(1 << 18) + 4).foreach { constant =>
      testSCM(constant, (1 << 14) - 1, mine = true) // max interval leads to 15 bits
      testSCM(constant, (1 << 13) + 1, mine = true) // min interval leads to 15 bits
      testSCM(constant, 0, mine = false) // flopoco design at 15 bits
    }
  }

  def writeFile(fileName: String, content: String) = {
    val f = new File(fileName)
    val updatedContent = Source.fromFile(f).getLines().toSeq :+ content
    val pw = new PrintWriter(f)
    pw.write(updatedContent.mkString("\n"))
    pw.close
  }

  val areaFile = "AreaReport.txt"
  val rtlFile = "RTLs.txt"

  def testSCM(constant: Int, range: Int, mine: Boolean) = {
    def doTest = {
      ChainsawDebug = true
      val report = if (mine)
        VivadoFlow(
          new Component {
            val input = in(UIntReal(range - 1))
            val sag = new SCM(input, constant)
            val output = out(sag.implicitValue)
          },
          "SCMMine",
          "synthWorkspace/SCMMine",
          force = true
        ).doit()
      else {
        VivadoFlow(
          new SCMWrapper(15, constant),
          "SCMMine",
          "synthWorkspace/SCMMine",
          force = true
        ).doit()
      }

      val handle = Source.fromFile("/home/ltr/IdeaProjects/Chainsaw/synthWorkspace/SCMMine/SCMMine.sv")
      val rtl = handle.getLines()
      writeFile(rtlFile, s"constant=$constant \n ${rtl.mkString("\n")}")
      writeFile(areaFile, s"${if (mine) "mine:" else "theirs"} constant=$constant LUT=${report.LUT}, FF=${report.FF}")
      handle.close()
    }

    try doTest
    catch {
      case _ => writeFile(areaFile, s"failed at constant = $constant")
    }
  }

  def testAddtion = {
    VivadoFlow(new Component {
      val a = in UInt (4 bits)
      val b = in UInt (5 bits)
      val output = out((a << 6) + b)
    },
      topModuleName = "testAddtion",
      workspacePath = synthWorkspace + "/testAddition"
    ).doit().printArea()
  }

  def testMultiplicationSignedness = {
    val unsigned = VivadoFlow(new Component {
      val a = in UInt (15 bits)
      val b = in UInt (15 bits)
      val output = out(a * b).addAttribute("use_dsp = \"no\"")
    },
      topModuleName = "testAddtion",
      workspacePath = synthWorkspace + "/temp").doit()

    val signed = VivadoFlow(new Component {
      val a = in SInt (16 bits)
      val b = in SInt (16 bits)
      val output = out(a * b).addAttribute("use_dsp = \"no\"")
    },
      topModuleName = "testAddtion",
      workspacePath = synthWorkspace + "/temp").doit()
    unsigned.printArea()
    signed.printArea()
  }
}
