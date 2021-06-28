package xilinx

import spinal.core._

case class VivadoTask(
                       verilogPostProcess: String => String = (s: String) => s,
                       frequencyTarget: HertzNumber = 200 MHz,
                       taskType: TaskType = SYNTH,
                       genSchematic: Boolean = true,
                       reportUtil: Boolean = true,
                       reportTiming: Boolean = true,
                       writeCheckpoint: Boolean = true
                     ) {}