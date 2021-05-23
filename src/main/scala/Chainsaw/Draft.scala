package Chainsaw

object Draft {

  def main(args: Array[String]): Unit = {
    import com.mathworks.engine
    import com.mathworks.engine.MatlabEngine
    import com.mathworks.matlab.types.Complex

    val testCase = (0 until 16).map(_.toDouble).toArray
    val testCase1 = Array.fill(16)(new Complex(1.0,1.0))
    val eng = MatlabEngine.startMatlab
    val ret = eng.feval("fft", testCase).asInstanceOf[Array[Double]]
    val ret1 = eng.feval("fft", testCase1).asInstanceOf[Array[Complex]]
    eng.close()
    println(ret.mkString(" "))
    println(ret1.mkString(" "))
//    println(ret.flatMap(complex => Array(complex.real, complex.imag)).mkString(" "))
  }

}
