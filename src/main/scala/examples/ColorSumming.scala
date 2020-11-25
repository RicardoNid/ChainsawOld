// design : 自定义Bundle上实现方法

package examples

import spinal.core._
import spinal.lib._

case class Color(channelWidth: Int) extends Bundle { // design : 自定义Bundle不仅可以定义接口,还可以实现方法
  val r = UInt(channelWidth bits)
  val g = UInt(channelWidth bits)
  val b = UInt(channelWidth bits)

  def +(that: Color): Color = { // design : 定义+方法 scala中方法和operator同质
    val result = Color(channelWidth)
    result.r := this.r + that.r
    result.g := this.g + that.g
    result.b := this.b + that.b
    result
  }

  def clear(): Color = {
    this.r := 0
    this.g := 0
    this.b := 0
    this
  }
}

class ColorSumming(sourceCount: Int, channelWidth: Int) extends Component {
  val io = new Bundle {
    val sources = in Vec(Color(channelWidth), sourceCount)
    val result = out(Color(channelWidth))
  }

  var sum = Color(channelWidth)
  sum.clear()
  for (i <- 0 until sourceCount) {
    sum \= sum + io.sources(i)
  }
  io.result := sum
}

object ColorSumming {
  def main(args: Array[String]): Unit = {
    SpinalSystemVerilog(new ColorSumming(8, 4))
  }
}
