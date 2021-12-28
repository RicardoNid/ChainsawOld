package Chainsaw.FTN

import spinal.core.{Bool, False, ImplicitArea, RegNext, assert, when}
import spinal.lib.Counter

class Trigger(val trigger: Bool, val count: Int) extends ImplicitArea[Bool] {
  require(count > 1)

  val start = False

  val counter = Counter(count)
  val triggered = counter =/= counter.getZero
  when(triggered || trigger)(counter.increment())

  override def implicitValue: Bool = triggered || RegNext(triggered)
}

object Trigger {
  def apply(trigger: Bool, count: Int): Trigger = new Trigger(trigger, count)
}