package Chainsaw.DSP.DAS
import spinal.core._
import spinal.lib._

case class FIR2Parallel(coff: Seq[Double]) extends Component {
  val io = new Bundle {
    val dataIn  = slave Flow (Vec(Para.dataType(), 2))
    val dataOut = master Flow (Vec(Para.dataType(), 2))
  }

  val h0   = coff.zipWithIndex.filter { case (c, i) => i % 2 == 0 }.map(_._1)
  val h1   = coff.zipWithIndex.filter { case (c, i) => i % 2 == 1 }.map(_._1)
  val h0h1 = h0.zip(h1).map { case (i, j) => i + j }

  val x0, x1, x01 = Flow(Para.dataType())
  Seq(x0, x1, x01).foreach(_.valid := io.dataIn.valid)
  Seq(x0, x1).zip(io.dataIn.payload).foreach { case (x, p) => x.payload := p }
  x01.payload := io.dataIn.payload.reduce(_ + _)

  val H0   = FIR(h0, x1)
  val H1   = FIR(h1, x0)
  val H0H1 = FIR(h0h1, x01)

  val y0, y1 = Flow(Para.dataType())
  Seq(y0, y1, io.dataOut).foreach(_.valid := io.dataIn.valid)
  val H0OutReg = Reg(Para.dataType()) init (0)
  when(io.dataIn.valid)(H0OutReg := H0.io.dataOut.payload)

  y0.payload := H1.io.dataOut.payload + H0OutReg
  y1.payload := H0H1.io.dataOut.payload - H0.io.dataOut.payload - H1.io.dataOut.payload

  Seq(y0, y1).zip(io.dataOut.payload).foreach { case (y, p) => p := y.payload }
}
