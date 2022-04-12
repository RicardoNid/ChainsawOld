package Chainsaw.DSP.DAS

import spinal.core._
import spinal.lib._
import Para._

case class Unwrap(rowCount: Int) extends Component {
  val io = new Bundle {
    val xn    = slave Flow (in(dataType()))
    val xnOut = master Flow (out(dataType()))
  }

  // column RAM
  val zero = Para.dataType()
  zero := 0
  val contents = Seq.fill(rowCount)(zero)
  val colRAM   = Mem(Para.dataType(), contents)
  val wAddr    = Counter(0, rowCount - 1)

  when(io.xn.valid)(wAddr.increment())

  val rAddr = Mux(wAddr.willOverflowIfInc, U(0), wAddr.value + 1)

  val loadCounter = Counter(0, rowCount - 1)
  when(io.xn.valid && ~loadCounter.willOverflowIfInc)(loadCounter.increment())
  val loadFull = loadCounter.willOverflowIfInc
  //--------------------unwrap logic--------------------------------------
  val xn1 = colRAM.readAsync(rAddr)
  val m   = xn1.raw(peakExp + resolutionExp downto resolutionExp)
  val v0  = xn1.raw.resize(resolutionExp).asUInt

  val n  = io.xn.payload.raw(peakExp + resolutionExp downto resolutionExp)
  val v1 = io.xn.payload.raw.resize(resolutionExp).asUInt

  val a = SInt(peakExp + 1 bits)
  val b = UInt(resolutionExp bits)
  b := v1

  val one = dataType()
  one := 1

  val mAddOrSumOne = Mux(v0 > v1, m + 1, m - 1)
  when(io.xn.payload + one <= xn1 || io.xn.payload >= xn1 + one) {
    when(m.lsb ^ n.lsb)(a := mAddOrSumOne) otherwise (a := m)
  } otherwise (a := n)
  //------------------------------------------------------------------------
  io.xnOut.payload.assignFromBits(a ## b)
  io.xnOut.valid := Mux(loadFull, io.xn.valid, False)
  val writeData = Mux(loadFull, io.xnOut.payload, io.xn.payload)
  colRAM.write(wAddr, writeData, io.xn.valid)
}

object Unwrap {
  def apply(rowCount: Int, xn: Flow[SFix]): Unwrap = {
    val ret = new Unwrap(rowCount)
    ret.io.xn := xn
    ret
  }
}
