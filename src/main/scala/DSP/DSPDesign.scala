package DSP

import spinal.core._

trait DSPDesign {
  val start: Bool

  def setStart(externalStart: Bool) = start := externalStart
}