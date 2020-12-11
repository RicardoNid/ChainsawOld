import spinal.core._
import spinal.lib.eda.xilinx._

val report = VivadoFlow(
  vivadoPath = "C:/Xilinx/Vivado/2020.1/bin",
  workspacePath = "C:/Users/lsfan/Documents/GitHub/anotherWork",
  toplevelPath = "FSMTemplate.sv",
  family = "Artix 7",
  device = "xc7k70t-fbg676-3",
  frequencyTarget = 1 MHz
)
println(report.getArea())
println(report.getFMax())