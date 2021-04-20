package DSP

import spinal.core._

trait DSPDesign {
  val start: Bool
  val busy: Bool

  def setStart(externalStart: Bool) = start := externalStart

  def isBusy() = busy
}