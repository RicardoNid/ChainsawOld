package xilinx

import org.apache.commons.io.FileUtils
import spinal.core._

import java.io.File
import java.nio.file.Paths
import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

/**
 */
class VivadoFlow[T <: Component](
                                  design: => T,
                                  topModuleName: String,
                                  workspacePath: String,
                                  vivadoConfig: VivadoConfig,
                                  vivadoTask: VivadoTask,
                                  force: Boolean = false,
                                  xdcPath: String = "",
                                  designPath: String = "",
                                  constraint: VivadoConstraint = VivadoConstraint() // no constraint by default
                                ) {

  import vivadoConfig._
  import vivadoTask._

  /**
   * Execute cmd command on host
   *
   * @param cmd : The cmd command to execute
   */
  private def doCmd(cmd: String): Unit = {
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd) !
    else
      Process(cmd) !
  }

  /**
   * Execute cmd command on host at given path
   *
   * @param cmd  The cmd command to execute
   * @param path Specifying the execution path
   */
  private def doCmd(cmd: String, path: String): Unit = { // do cmd at the workSpace
    println(cmd)
    if (isWindows)
      Process("cmd /C " + cmd, new java.io.File(path)) !
    else
      Process(cmd, new java.io.File(path)) !
  }

  /**
   * write content in dedicated file
   *
   * @param fileName name of the file
   * @param content  content to be write
   */
  private def writeFile(fileName: String, content: String) = {
    val tcl = new java.io.FileWriter(Paths.get(workspacePath, fileName).toFile)
    tcl.write(content)
    tcl.flush()
    tcl.close()
  }

  val isWindows = System.getProperty("os.name").toLowerCase().contains("win")

  /**
   * Generate tcl script content for Vivado Flow
   *
   * {{{The generated tcl script will consists of these commands:}}}
   * {{{    read_verilog -sv xxx.sv (or read_verilog, read_vhdl)}}}
   * {{{    read_xdc xxx.xdc}}}
   * {{{    synth_design -part xxx -top xxx}}}
   * {{{    opt_design (for implementation)}}}
   * {{{    place_design (for implementation)}}}
   * {{{    route_design (for implementation)}}}
   * {{{    write_bitstream -force xxx.bit (for implementation)}}}
   * {{{    report_utilization}}}
   * {{{    report_timing}}}
   * {{{    write_checkpoint xxx.dcp}}}
   *
   * @param vivadoConfig you can specify your Vivado Configurations at src/main/scala/Xilinx/package.scala
   * @param rtlSources   path of your RTL source codes
   * @return tcl script
   */
  def getScript(vivadoConfig: VivadoConfig, rtlSources: mutable.LinkedHashSet[String]) = {
    var script = ""

    if (designPath.isEmpty) rtlSources.map(_.replace(workspacePath + "/", "")).foreach { path =>
      println(designPath)
      if (path.endsWith(".sv")) script += s"read_verilog -sv $path \n"
      else if (path.endsWith(".v")) script += s"read_verilog $path \n"
      else if (path.endsWith(".vhdl") || path.endsWith(".vhd")) script += s"read_vhdl $path \n"
      else if (path.endsWith(".bin")) Unit
      else throw new IllegalArgumentException(s"invalid RTL source path $path")
    }
    else {
      script += s"read_verilog -v $designPath \n"
    }

    script += (if (xdcPath.nonEmpty) s"read_xdc $xdcPath\n" else s"read_xdc doit.xdc\n")

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
        script += s"write_checkpoint ${topModuleName}_after_place.dcp\n"
        script += "route_design\n"
        script += s"write_checkpoint ${topModuleName}_after_route.dcp\n"
        script += s"write_bitstream -force ${topModuleName}.bit\n"
        taskName = "impl"
      }
    }
    // util & timing can't be reported before synthesis
    if (reportUtil && taskType != ELABO) script += s"report_utilization\n"
    if (reportTiming && taskType != ELABO) script += s"report_timing\n"
    if (writeCheckpoint && taskType != ELABO) script += s"write_checkpoint ${topModuleName}_${taskName}.dcp\n"
    script
  }

  /**
   * generate XDC constraints content for Vivado Flow
   *
   * @return XDC constraints
   */
  def getXdc = {
    val targetPeriod = frequencyTarget.toTime
    s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
  }

  /**
   * @return VivadoReport as an object
   */
  def doit() = {

    // prepare the workspace
    val workspacePathFile = new File(workspacePath)
    if (workspacePathFile.listFiles != null) {
      assert(force, "the workspace is not empty, to flush it anyway, using \"force = true\"")
      FileUtils.deleteDirectory(workspacePathFile)
    }

    //    workspacePathFile.mkdir()    // this may lead to bug
    Process(s"mkdir -p $workspacePath") ! // create directory in this way instead

    // generate systemverilog
    val spinalReport = SpinalConfig(targetDirectory = workspacePath).generateSystemVerilog(design.setDefinitionName(topModuleName))

    writeFile("doit.tcl", getScript(vivadoConfig, spinalReport.rtlSourcesPaths))
    writeFile("doit.xdc", getXdc)

    doCmd(s"$vivadoPath/vivado -nojournal -log doit.log -mode batch -source doit.tcl", workspacePath)


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
                             force: Boolean = true,
                             xdcPath: String = "",
                             designPath: String = ""
                           ) = new VivadoFlow(design, topModuleName, workspacePath, vivadoConfig, vivadoTask, force, xdcPath, designPath)
}