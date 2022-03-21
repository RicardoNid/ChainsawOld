import Chainsaw._
import breeze.linalg._
import breeze.math._
import breeze.numerics._
import breeze.numerics.constants._
import breeze.signal._


val a = 21.8
val b = scala.math.ceil(a) -a
val c =a - scala.math.floor(a)
val theta = Range(0, 4).map(i => 1 / BigInt(2).pow(i).toDouble)