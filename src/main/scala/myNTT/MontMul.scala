package myNTT

import spinal.core._

import scala.language.postfixOps

//蒙哥马利乘法器函数
//delay：n+1

object MontMul {
  def apply(A: UInt /*输入的第一个数据*/, B: UInt /*输入的第二个数据*/, N: BigInt /*被模常数*/ ) = new Area {
    val n             = log2Up(N) //A与B的位宽
    val N_add_1_div_2 = (N + 1) >> 1
    val aNext         = Array.fill(n)(UInt(n + 1 bits))
    val aTemp         = Array.fill(n - 1)(UInt(n + 2 bits))
    val bNext         = new Array[UInt](n - 1)
    val ANext         = new Array[UInt](n - 1)
    aNext(0) := RegNext(Mux(B.lsb, A(n - 1 downto 1) +^ Mux(A.lsb, N_add_1_div_2, 0), U(0))).resize(n + 1)
    bNext(0) = RegNext(B(n - 1 downto 1))
    ANext(0) = RegNext(A)
    for (i <- 0 until n - 1) {
      aTemp(i)     := aNext(i) +^ Mux(bNext(i).lsb, ANext(i), 0)
      aNext(i + 1) := RegNext(aTemp(i)(n + 1 downto 1) + Mux(aTemp(i).lsb, N_add_1_div_2, 0))
      if (i != n - 2) {
        bNext(i + 1) = RegNext(bNext(i)(n - 2 - i downto 1))
        ANext(i + 1) = RegNext(ANext(i))
      }
    }
    val aFinal = aNext.last - N
    val output = RegNext(Mux(aFinal.msb, aNext.last(n - 1 downto 0), aFinal(n - 1 downto 0)))
  }
}
