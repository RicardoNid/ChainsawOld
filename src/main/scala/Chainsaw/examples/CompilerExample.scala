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

  val gComb0, gComb1, gComb2, gComb3, gComb4, gComb5 = UInt(4 bits)
  val g0, g1, g2, g3, g4, g5 = Reg(UInt(4 bits))
  val gs = Seq(gComb0, gComb1, gComb2, gComb3, gComb4, gComb5)
  gs.zip(Seq(g0, g1, g2, g3, g4, g5)).foreach { case (comb, reg) => reg := comb }

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
  gComb0 := e + f
  // case 2, inhomogeneous - shortest path
  gComb1 := a + e
  // case 3, self-loop - shortest path
  gComb2 := g2 + e + f
  // case 4, go through a RAM whose write2read delay = 2
  gComb3 := m
  // case 5, go through a RAM whose write2read delay = 1
  gComb4 := n
  // case 6, go through a RAM with a readWrite port
  gComb5 := l

  //  println(s"latency = ${MyLatencyAnalysis(a, g)}")
  gs.zipWithIndex.foreach{ case(g,i) =>
    printlnYellow(s"case $i")
    val mine = MyLatencyAnalysis(a, g)
    val ori = LatencyAnalysis(a, g)
    assert(mine == ori)
  }
}

object CompilerExample extends App {
  GenRTL(CompilerExample())
}
