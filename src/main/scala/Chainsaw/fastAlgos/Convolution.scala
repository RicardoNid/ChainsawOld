package Chainsaw.fastAlgos

import Chainsaw._
import breeze.linalg._
import breeze.math.Complex
import breeze.signal.{OptMethod, OptOverhang, OptPadding, OptRange, convolve}
import breeze.signal.support.CanConvolve

object Convolution {

  def rawCorrelation(data: DenseVector[Complex], kernel: DenseVector[Complex], range: Range): DenseVector[Complex] = {
    val N = data.length
    val L = kernel.length
    require(N >= range.end + L - 1)
    val ret = range.map(i => sum(data(i until i + L) *:* kernel))
    new DenseVector(ret.toArray)
  }

  implicit def dvC1DConvolve: CanConvolve[DenseVector[Complex], DenseVector[Complex], DenseVector[Complex]] = {
    new CanConvolve[DenseVector[Complex], DenseVector[Complex], DenseVector[Complex]] {
      override def apply(data: DenseVector[Complex], kernel: DenseVector[Complex],
                         range: OptRange,
                         correlate: Boolean,
                         overhang: OptOverhang,
                         padding: OptPadding,
                         method: OptMethod): DenseVector[Complex] = {

        val N = data.length
        val L = kernel.length
        require(N >= L)

        val padLength = overhang match {
          case OptOverhang.Full => 2 * (L - 1)
          case OptOverhang.None => 0
          case OptOverhang.PreserveLength => L - 1
        }

        val padLengthL = padLength / 2
        val padLengthR = padLength - padLengthL

        val (padL, padR) = padding match {
          case OptPadding.Boundary => (DenseVector.fill(padLengthL)(data(0)), DenseVector.fill(padLengthR)(data(-1)))
          case OptPadding.Cyclical => (data(N - padLengthL to N - 1), data(0 until padLengthR))
          case OptPadding.ValueOpt(value) => (DenseVector.fill(padLengthL)(value.asInstanceOf[Complex]), DenseVector.fill(padLengthR)(value.asInstanceOf[Complex]))
          case OptPadding.Zero => (DenseVector.zeros[Complex](padLengthL), DenseVector.zeros[Complex](padLengthR))
        }

        val paddedData = DenseVector.vertcat(padL, data, padR)
        val fullRange = paddedData.length - L + 1

        val parsedRange = range match {
          case OptRange.All => 0 until fullRange
          case OptRange.RangeOpt(r) => r
        }

        if (correlate) rawCorrelation(paddedData, kernel, parsedRange)
        else rawCorrelation(paddedData, reverse(kernel), parsedRange)
      }
    }
  }

  def linearConvolve(data: DenseVector[Complex], kernel: DenseVector[Complex]) =
    convolve(data, kernel, overhang = OptOverhang.Full)

  def cyclicConvolve(data: DenseVector[Complex], kernel: DenseVector[Complex]) = {
    require(data.length == kernel.length)
    convolve(data, kernel, padding = OptPadding.Cyclical, overhang = OptOverhang.Full, range = 0 until data.length)
  }
}
