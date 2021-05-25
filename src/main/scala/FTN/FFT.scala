package FTN

import Chainsaw._
import breeze.numerics.{abs, pow}
import com.mathworks.matlab.types.Complex
import spinal.core._
import spinal.core.sim._

class FFT(input: Vec[Real], inverse: Boolean = false) extends DSPArea[Vec[Real], Array[Complex], Array[Complex]] {
  // TODO: implement
  override def timing: TimingInfo = TimingInfo(1, 1, 1, 1)

  override def referenceModel(testCase: Array[Complex]): Array[Complex] = {
    val functionName = if (inverse) "ifft" else "fft"
    eng.feval(functionName, testCase).asInstanceOf[Array[Complex]]
  }

  val N = input.length / 2

  val inputComplexes = (0 until N).map(i => ComplexReal(input(2 * i), input(2 * i + 1)))

  def coeffW(kn: Int) = {
    import breeze.numerics._
    import breeze.numerics.constants.Pi
    printlnYellow(s"coeffW($kn) being invoked")
    val exp = (-2) * Pi * kn / N
    val real = cos(exp)
    val imag = sin(exp)
    printlnYellow(s"real: $real, imag: $imag")
    val realReal = QFormatReal(Q"1Q15")
    println(s"error from coeff ${realReal.error}")
    realReal := real
    val imagReal = QFormatReal(Q"1Q15")
    println(s"error from coeff ${imagReal.error}")
    imagReal := imag
    ComplexReal(realReal, imagReal)
  }

  // TODO: Implement
  override def implicitValue: Vec[Real] = {
    val ret = if (isPow2(N)) { // using butterfly
      val layer = log2Up(N)

      def indexReverse(index: Int): Int = index
        .toBinaryString.reverse.padTo(layer, '0').reverse
        .zipWithIndex.map { case (c, i) => c.asDigit * (1 << i) }.sum

      def build(input: Seq[ComplexReal], layerRemained: Int): Seq[ComplexReal] = {
        require(layerRemained >= 1)

        printlnGreen(s"${input.length / 2}")
        layerRemained match {
          case 1 => Seq(input(0) + input(1), (input(0) - input(1)) * coeffW(0))
          case _ => {
            val half = input.length / 2
            val upper = input.take(half)
            val lower = input.takeRight(half)
            val midUpper = upper.zip(lower).map { case (up, low) => up + low }
            val midLower = upper.zip(lower).map { case (up, low) => up - low }
            printlnRed(s"lower length ${midLower.length}")
            val midLowerMultiplied = midLower.zipWithIndex.map { case (complexReal, i) =>
              val kn = i * (1 << (layer - layerRemained))
              complexReal * (if (inverse) coeffW(-kn) else coeffW(kn))
            }
            val ret = midUpper ++ midLowerMultiplied
            build(ret.take(half), layerRemained - 1) ++ build(ret.takeRight(half), layerRemained - 1)
          }
        }
      }

      printlnRed(s"N is $N")
      val disorded = build(inputComplexes, layer)
      (0 until N).map(i => disorded(indexReverse(i)))
        .flatMap(complexReal => Vec(complexReal.real, complexReal.imag))
        .map(real => if (inverse) real >> layer else real)
    }
    else {
      // TODO
      throw new IllegalArgumentException("not implemented yet")
    }

    println(s"final errors ${ret.map(_.error).mkString(" ")}")
    RegNext(Vec(ret))
  }
}

class FFTDUT(N: Int, inverse: Boolean) extends DSPDUTTiming[Vec[Real], Vec[Real]] {
  override val input: Vec[Real] = if (inverse) in(in Vec(RealWithError(-1.5 * N, 2.5 * N, (-15 + log2Up(N)) exp), N * 2))
  else in Vec(RealWithError(-1.5, 2.5, -15 exp), N * 2)
  val fft = new FFT(input, inverse)
  override val output: Vec[Real] = out(fft.implicitValue)
  override val timing: TimingInfo = fft.timing
}






