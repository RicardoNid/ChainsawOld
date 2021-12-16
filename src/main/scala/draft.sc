import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

val vec = DenseVector(1,2,3,4)

val mat = new DenseMatrix(3,2, Array(1,2,3,4,5,6))

val factor = Complex(1,0) / Complex(0.30871945533265976, 0.27707849007413665)
Complex(0.7275636800328681, 0.6832234717598454) * factor
