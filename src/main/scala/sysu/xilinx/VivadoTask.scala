package sysu.xilinx

import spinal.core._

sealed trait TaskType

object ELABO extends TaskType

object SYNTH extends TaskType

object IMPL extends TaskType

case class VivadoTask(
                       topModuleName: String,
                       verilogPostProcess: String => String = (s: String) => s,
                       workspacePath: String = "",
                       frequencyTarget: HertzNumber = 400 MHz,
                       taskType: TaskType = SYNTH,
                       genSchematic: Boolean = true,
                       reportUtil: Boolean = true,
                       reportTiming: Boolean = true,
                       writeCheckpoint: Boolean = true
                     ) {

  def getScript(vivadoConfig: VivadoConfig) = {
    var script = s"read_verilog -sv ${topModuleName}.sv\n read_xdc doit.xdc\n"
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
    if (reportUtil && taskType != ELABO) script += s"report_utilization\n"
    if (reportTiming && taskType != ELABO) script += s"report_timing\n"
    if (writeCheckpoint && taskType != ELABO) script += s"write_checkpoint ${topModuleName}_${taskName}.dcp\n"
    script
  }

  def getXdc() = {
    val targetPeriod = frequencyTarget.toTime
    s"""create_clock -period ${(targetPeriod * 1e9) toBigDecimal} [get_ports clk]"""
  }
}