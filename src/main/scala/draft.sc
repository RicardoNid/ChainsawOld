import breeze.linalg._
import breeze.numerics._
import breeze.signal._

val reuse = DenseMatrix.eye[Int](5)

val cols = (0 until 5).map(i => reuse(::, i))

cols(1) == cols(2)

cols.





