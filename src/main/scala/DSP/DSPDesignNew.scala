package DSP

import spinal.core._

trait DSPDesignNew {
  val start: Bool

  def setStart(externalStart: Bool) = start := externalStart
}