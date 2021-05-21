import com.mathworks.engine._
import com.mathworks.matlab.types._

object HelloMatlab {
  @throws[Exception]
  def main(args: Array[String]): Unit = {

    //    CreateStruct.doit()
    //    val eng = MatlabEngine.startMatlab
    //    val a = Array(0.1, 0.2, 0.3)
    //    val roots = eng.feval("qfuncinv", a).asInstanceOf[Array[Double]]
    //    println(roots.mkString(" "))
    //
    //    val c = new Complex(2, 3)
    //    val d: Complex = eng.feval("conj", c)
    //    println(d.real, d.imag)
    //
    //    eng.close()

    //    val addition = "function [result] = Add(number)\n    result = sum(number);"
    val eng = MatlabEngine.startMatlab
    eng.eval("cd '/home/ltr'")
    //    eng.eval(addition)
    val result: Double = eng.feval("Add", Array(1.0, 2.0))
    println(result)
    eng.close()
  }
}

