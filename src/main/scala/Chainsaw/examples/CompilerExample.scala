package Chainsaw.examples

import Chainsaw._
import spinal.core._
import spinal.lib._

case class CompilerExample() extends Component {
  val a, b, c, d = in UInt (4 bits)

  val eComb = a + b
  val e = Reg(UInt(4 bits))
  e := eComb

  val fComb = c + d
  val f = Reg(UInt(4 bits))
  f := fComb

  val gComb = UInt(4 bits)
  val g = Reg(UInt(4 bits))
  g := gComb

  val m = UInt(4 bits)
  val n = UInt(4 bits)
  val l = UInt(4 bits)

  val addr = in UInt (4 bits)

  val ram = Mem(UInt(4 bits), 16)
  ram(addr) := a
  m := ram.readSync(addr)

  val ram1 = Mem(UInt(4 bits), 16)
  ram1(addr) := a
  n := ram1.readAsync(addr)

  val ram2 = Mem(UInt(4 bits), 16)
  l := ram2.readWriteSync(addr, a, True, b.msb)

  // case 1
  //  gComb := e + f
  // case 2, inhomogeneous - shortest path
  //  gComb := a + e
  // case 3, self-loop - shortest path
  //  gComb := g + e + f
  // case 4, go through a RAM whose write2read delay = 2
  gComb := m
  // case 5, go through a RAM whose write2read delay = 1
  //  gComb := n
  // case 6, go through a RAM with a readWrite port
  //  gComb := l

  //  println(s"latency = ${MyLatencyAnalysis(a, g)}")
  println(s"latency = ${MyLatencyAnalysis(a, g)}")
  println(s"latency = ${LatencyAnalysis(a, g)}")
}

object CompilerExample extends App {
  GenRTL(CompilerExample())
}
