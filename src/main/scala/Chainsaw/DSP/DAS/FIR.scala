package Chainsaw.DSP.DAS
import spinal.core._
import spinal.lib._

/** @param coff
  *   y[n] = x[n] * coff[L - 1] + x[n - 2] * coff[L -2] + ...
  */
class FIR(coff: Seq[Double]) extends Component {
  val io = new Bundle {
    val dataIn  = slave Flow (Para.dataType())
    val dataOut = master Flow (Para.dataType())
  }

  val coffSFix = coff.map { c =>
    val C = Para.dataType()
    C := c
    C
  }
  val products          = coffSFix.map(io.dataIn.payload * _)
  val productsTruncated = Seq.fill(coff.size)(Para.dataType())
  productsTruncated.zip(products).foreach { case (t, p) => t := p.truncated }
  val regs = Seq.fill(coff.length - 1)(Reg(Para.dataType) init (0))

  when(io.dataIn.valid) {
    val regsZip = regs.tail.zip(productsTruncated.tail.init) zip (regs.init)
    regsZip.foreach { case ((rn, p), r) => rn := r + p.truncated }
    regs.head := productsTruncated.head
  }

  io.dataOut.valid   := io.dataIn.valid
  io.dataOut.payload := regs.last + productsTruncated.last

}

object FIR {
  def apply(coff: Seq[Double], input: Flow[SFix]): FIR = {
    val f = new FIR(coff)
    f.io.dataIn := input
    f
  }

}
