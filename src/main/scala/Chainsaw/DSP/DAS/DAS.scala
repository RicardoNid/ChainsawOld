package Chainsaw.DSP.DAS

import Chainsaw.DSP.{CORDIC, CordicConfig}
import Chainsaw.{DSPDUTTiming, TimingInfo}
import spinal.core._
import spinal.lib._

object Para {
  val filterOrderNum         = 500
  val peakExp, resolutionExp = 8
  val dataType               = HardType(SFix(peakExp exp, -resolutionExp exp))
  val gauge                  = 250
  val rowCount               = 320000
  val rowCount2              = 1279
}
