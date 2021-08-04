package FTN

import Chainsaw._
import matlabIO._
import spinal.core._

case class QAMModFTN(bitAlloc: Array[Int]) extends Component {

  val bitAllocatedMax = bitAlloc.max
  val parallelFactor = bitAlloc.length

  val dataIn = in Bits (bitAlloc.sum bits)
  val dataOut = out Vec(Bits(12 bits), parallelFactor)

  // TODO: avoid high fan-out
  val eng = AsyncEng.get()

  val QAMValues: Seq[Seq[MComplex]] = (1 to bitAllocatedMax).map { i => // Normalization done here
    val M = 1 << i
    val values = (0 until M).toArray
    i match {
      case 1 => Seq(1, -1).map(new MComplex(_, 0))
      case _ =>
        val symbols = eng.feval[Array[MComplex]]("qammod", values, Array(M))
        val sum = symbols.map(complex => complex.real * complex.real + complex.imag * complex.imag).sum
        val rms = scala.math.sqrt(sum / symbols.size)
        symbols.toSeq.map(complex => new MComplex(complex.real / rms, complex.imag / rms))
    }
  }

  println(QAMValues.map(_.mkString(" ")).mkString("\n"))

  // TODO: merge them together
  val QAMROMReals = QAMValues.map(values => Mem(values.map(complex => SF(complex.real, 1 exp, 6 bits))))
  val QAMROMImags = QAMValues.map(values => Mem(values.map(complex => SF(complex.imag, 1 exp, 6 bits))))

  val ends = (0 until parallelFactor).map(i => bitAlloc.take(i + 1).sum)
  val starts = 0 +: ends.init
  val segments = ends.zip(starts).map { case (end, start) =>
    bitAlloc.sum - 1 - start downto bitAlloc.sum - end
  }

  dataOut.zip(bitAlloc.zip(segments)).foreach { case (output, (bitAllocated, segment)) =>
    bitAllocated match {
      case 0 => // do nothing
      case _ => output :=
        QAMROMReals(bitAllocated - 1)(dataIn(segment).asUInt).asBits ##
          QAMROMImags(bitAllocated - 1)(dataIn(segment).asUInt).asBits
    }
  }
}

object QAMModFTN extends App {
  val originalAllocation = Array.fill(128)(4)
  GenRTL(new QAMModFTN(originalAllocation))
  VivadoSynth(new QAMModFTN(originalAllocation), name = "QAM")
}
