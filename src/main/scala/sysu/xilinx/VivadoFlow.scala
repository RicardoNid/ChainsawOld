package sysu.xilinx

import java.io.File
import java.nio.file.Paths

import org.apache.commons.io.FileUtils
import spinal.core._

import scala.io.Source
import scala.sys.process._


class VivadoFlow[T <: Component](
                                  design: => T,
                                  vivadoConfig: VivadoConfig,
                                  vivadoTask: VivadoTask,
                                  force: Boolean = false
                                ) {

  def doCmd(cmd: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  def doCmd(cmd: String, path: String): Unit = { // do cmd at the workSpace
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !
  }

  import vivadoConfig._
  import vivadoTask._

  def writeFile(fileName: String, content: String) = {
    val tcl = new java.io.FileWriter(Paths.get(workspacePath, fileName).toFile)
    tcl.write(content)
    tcl.flush();
    tcl.close();
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")


  def doit() = {

    // prepare the workspace
    val workspacePathFile = new File(workspacePath)
    if (workspacePathFile.listFiles != null) {
      assert(force, "the workspace is not empty, to flush it anyway, using \"force = true\"")
      FileUtils.deleteDirectory(workspacePathFile)
    }
    workspacePathFile.mkdir()
    // generate systemverilog and do post processing
    SpinalConfig(targetDirectory = workspacePath).generateSystemVerilog(design.setDefinitionName(topModuleName))
    val verilogContent = Source.fromFile(Paths.get(workspacePath, s"${topModuleName}.sv").toFile).getLines.mkString("\n")
    val newVerilogContent = verilogPostProcess(verilogContent)
    writeFile(s"${topModuleName}.sv", newVerilogContent)

    val source = new java.io.FileWriter(Paths.get(workspacePath, s"${topModuleName}.sv").toFile)
    source.write(newVerilogContent)
    source.flush();
    source.close();

    writeFile("doit.tcl", vivadoTask.getScript(vivadoConfig))
    writeFile("doit.xdc", vivadoTask.getXdc())

    doCmd(s"$vivadoPath/vivado -nojournal -log doit.log -mode batch -source doit.tcl", workspacePath)

    new VivadoReport(workspacePath, deviceFamily, frequencyTarget)
  }
}

object VivadoFlow {
  def apply[T <: Component](
                             design: => T,
                             vivadoConfig: VivadoConfig,
                             vivadoTask: VivadoTask,
                             force: Boolean = false
                           ) = new VivadoFlow(design, vivadoConfig, vivadoTask, force)
}