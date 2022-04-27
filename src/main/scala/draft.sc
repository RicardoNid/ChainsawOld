import Chainsaw.algos.Qam
import breeze.linalg.DenseVector


def take(s:String) = if(s.startsWith("-")) s.take(7) else s.take(6)

println(Qam.qammod(DenseVector(Array.range(0, 16)), 16).toArray
  .map(complex => s"Complex(${take(complex.real.toString)}, ${take(complex.imag.toString)})").mkString(","))