val N = 16
val n = 3
val k = 4
import breeze.numerics._
import breeze.numerics.constants.Pi
val exp = (-2) * Pi * k * n / N
val real = cos(exp)
val imag = sin(exp)
