package Chainsaw.algos

import breeze.linalg._
import breeze.math.{Complex, _}
import breeze.signal.{OptOverhang, OptPadding, OptRange, convolve}

import scala.reflect.ClassTag

/** definition of generic convolution, linear and cyclic convolution
  */
object Convolution {

  // TODO: find reference using concepts below(or, write one by myself)

  /** general template for all kinds of convolutions & correlations
    *
    * @param data
    *   data sequence
    * @param kernel
    *   kernel/coeff sequence
    * @param correlate
    *   convolution/correlation
    * @param overhang
    *   determine the padded length
    * @param padding
    *   determine the content to pad
    * @param range
    *   determine the range of output
    */
  @definition
  def genericConvolve[T](
      data: DenseVector[T],
      kernel: DenseVector[T],
      correlate: Boolean    = false,
      overhang: OptOverhang = OptOverhang.Full,
      padding: OptPadding   = OptPadding.Zero,
      range: OptRange       = OptRange.All
  )(implicit semiring: Semiring[T], tag: ClassTag[T]): DenseVector[T] = {

    val N = data.length
    val L = kernel.length
    require(N >= L)

    def innerCorrelation(paddedData: DenseVector[T], kernel: DenseVector[T], range: Range): DenseVector[T] = {
      val N = paddedData.length
      require(N >= range.end + L - 1, s"inner correlation with data length ${paddedData.length}, kernel length ${kernel.length} and range $range")
      val ret = range.map(i => sum(paddedData(i until i + L) *:* kernel))
      new DenseVector(ret.toArray)
    }

    val padLength = overhang match {
      case OptOverhang.Full           => 2 * (L - 1)
      case OptOverhang.None           => 0
      case OptOverhang.PreserveLength => L - 1
    }

    val padLengthL = padLength / 2
    val padLengthR = padLength - padLengthL

    val (padL, padR) = padding match {
      case OptPadding.Boundary        => (DenseVector.fill(padLengthL)(data(0)), DenseVector.fill(padLengthR)(data(-1)))
      case OptPadding.Cyclical        => (data(N - padLengthL to N - 1), data(0 until padLengthR))
      case OptPadding.ValueOpt(value) => (DenseVector.fill(padLengthL)(value.asInstanceOf[T]), DenseVector.fill(padLengthR)(value.asInstanceOf[T]))
      case OptPadding.Zero            => (DenseVector.zeros[T](padLengthL), DenseVector.zeros[T](padLengthR))
    }

    val paddedData = DenseVector.vertcat(padL, data, padR)
    val fullRange  = paddedData.length - L + 1

    val parsedRange = range match {
      case OptRange.All         => 0 until fullRange
      case OptRange.RangeOpt(r) => r
    }

    if (correlate) innerCorrelation(paddedData, kernel, parsedRange)
    else innerCorrelation(paddedData, reverse(kernel), parsedRange)
  }

  def genericLinearConvolve[T](data: DenseVector[T], kernel: DenseVector[T])(implicit semiring: Semiring[T], tag: ClassTag[T]): DenseVector[T] =
    genericConvolve(data, kernel)

  def genericCyclicConvolve[T](data: DenseVector[T], kernel: DenseVector[T])(implicit semiring: Semiring[T], tag: ClassTag[T]): DenseVector[T] = {
    require(data.length == kernel.length)
    genericConvolve(data, kernel, padding = OptPadding.Cyclical, range = 0 until data.length)
  }
}
