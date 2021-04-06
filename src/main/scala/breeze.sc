import breeze.linalg.DenseVector
import breeze.math.Complex
import breeze.signal._

// 1D FFT on complex number
val temp = fourierTr.dvComplex1DFFT(DenseVector(Array(Complex(1, 2), Complex(3, 4))))
val temp = fourierTr.dvDouble1DFFT(DenseVector(Array(1.0, 2.0, 3.0, 4.0)))
val temp = filter(DenseVector(Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0)), DenseVector(Array(1.0, 2.0)))