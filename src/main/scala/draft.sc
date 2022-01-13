def dbc(n:Double, d:Double) = {
  // d is close to 1, n is within [0.5, 2)
  var x = n
  var t = d
  (0 until 12).foreach{_ =>
    val f = 2 - t
    x = x * f
    t = t * f
  }
  x
}

import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._

abs(Complex(1,1))
