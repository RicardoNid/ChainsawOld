package xilinx

import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import java.nio.file.Paths
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

class VivadoFlow[T <: Component](
                                  design: => T,
                                  topModuleName: String,
                                  workspacePath: String,
                                  vivadoConfig: VivadoConfig,
                                  vivadoTask: VivadoTask,
                                  force: Boolean = false
                                ) {

  import vivadoConfig._
  import vivadoTask._

  private def doCmd(cmd: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  private def doCmd(cmd: String, path: String): Unit = { // do cmd at the workSpace
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !
  }

  private def writeFile(fileName: String, content: String) = {
    val tcl = new java.io.FileWriter(Paths.get(workspacePath, fileName).toFile)
    tcl.write(content)
    tcl.flush()
    tcl.close()
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  def getScript(vivadoConfig: VivadoConfig, rtlSources: mutable.LinkedHashSet[String]) = {
    var script = ""

    rtlSources.map(_.replace(workspacePath + "/", "")).foreach { path =>
      if (path.endsWith(".sv")) script += s"read_verilog -sv $path \n"
      else if (path.endsWith(".v")) script += s"read_verilog $path \n"
      else if (path.endsWith(".vhdl") || path.endsWith(".vhd")) script += s"read_vhdl $path \n"
      else if (path.endsWith(".bin")) Unit
      else throw new IllegalArgumentException(s"invalid RTL source path $path")
    }

    script += s"read_xdc doit.xdc\n"

    var taskName = ""
    taskType match {
      case ELABO => {
        script += s"synth_design -part ${vivadoConfig.devicePart} -top ${topModuleName} -rtl\n"
        taskName = "elabo"
      }
      case SYNTH => {
        script += s"synth_design -part ${vivadoConfig.devicePart} -top ${topModuleName}\n"
        taskName = "synth"
      }
      case IMPL => {
        script += s"synth_design -part ${vivadoConfig.devicePart} -top ${topModuleName}\n"
        script += "opt_design\n"
        script += "place_design\n"
        script += "route_design\n"
        taskName = "impl"
      }
    }
    // util & timing can't be reported before synthesis
    if (reportUtil && taskType != ELABO) script += s"report_utilization\n"
    if (reportTiming && taskType != ELABO) script += s"report_timing\n"
    if (writeCheckpoint && taskType != ELABO) script += s"write_checkpoint ${topModuleName}_${taskName}.dcp\n"
    script
  }

  def getXdc = {
    val targetPeriod = frequencyTarget.toTime
    s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
  }

  def doit() = {

    // prepare the workspace
    val workspacePathFile = new File(workspacePath)
    if (workspacePathFile.listFiles != null) {
      assert(force, "the workspace is not empty, to flush it anyway, using \"force = true\"")
      FileUtils.deleteDirectory(workspacePathFile)
    }
    workspacePathFile.mkdir()
    // generate systemverilog and do post processing
    //    val spinalReport = SpinalConfig(targetDirectory = workspacePath).generateVhdl(design.setDefinitionName(topModuleName))
    val spinalReport = SpinalConfig(targetDirectory = workspacePath).generateSystemVerilog(design.setDefinitionName(topModuleName))

    // FIXME: posprocess
    //    val verilogContent = Source.fromFile(Paths.get(workspacePath, s"${topModuleName}.sv").toFile).getLines.mkString("\n")
    //    val newVerilogContent = verilogPostProcess(verilogContent)
    //    writeFile(s"${topModuleName}.sv", newVerilogContent)
    //    val source = new java.io.FileWriter(Paths.get(workspacePath, s"${topModuleName}.sv").toFile)
    //    source.write(newVerilogContent)
    //    source.flush();
    //    source.close();

    writeFile("doit.tcl", getScript(vivadoConfig, spinalReport.rtlSourcesPaths))
    writeFile("doit.xdc", getXdc)

    doCmd(s"${vivadoPath}/vivado -nojournal -log doit.log -mode batch -source doit.tcl", workspacePath)

    new VivadoReport(workspacePath, xilinxDeviceFamily, frequencyTarget)
  }
}

// TODO: modify the API, name and path should be passed to the Flow, so the user can use Config and Task from the recommendation
object VivadoFlow {
  def apply[T <: Component](
                             design: => T,
                             topModuleName: String,
                             workspacePath: String,
                             vivadoConfig: VivadoConfig = defaultVivadoConfig,
                             vivadoTask: VivadoTask = VivadoTask(),
                             force: Boolean = true
                           ) = new VivadoFlow(design, topModuleName, workspacePath, vivadoConfig, vivadoTask, force)
}