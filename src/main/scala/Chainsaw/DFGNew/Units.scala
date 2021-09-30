package Chainsaw.DFGNew

case class Cycle(value: Int) {
  def +(that: Cycle) = Cycle(this.value + that.value)
  def -(that: Cycle) = Cycle(this.value - that.value)
  def *(that: Cycle) = Cycle(this.value * that.value)
  def /(that: Cycle) = Cycle(this.value / that.value)
  def %(that: Cycle) = Cycle(this.value % that.value)
}
