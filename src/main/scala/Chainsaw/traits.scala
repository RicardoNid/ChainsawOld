package Chainsaw

case class TimingInfo(inputInterval: Int, outputInterval: Int, latency: Int, initiationInterval: Int)

trait Testable {
  val getTimingInfo: TimingInfo
}
