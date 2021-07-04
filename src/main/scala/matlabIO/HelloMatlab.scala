package matlabIO

import com.mathworks.engine.MatlabEngine
import com.mathworks.matlab.types._
import scala.collection.JavaConversions._

object HelloMatlab {
  @throws[Exception]
  def main(args: Array[String]): Unit = {
    // start session
    val eng = MatlabEngine.startMatlab
    eng.eval(s"cd ${matlabWorkingSpace.toString}")

    // generally, use array for vectors, use double for elements
    // put and get variable
    eng.putVariable("a", Array.tabulate(2, 3)(_ + _))
    eng.eval(s"b = sum(a)")
    val c: Array[Double] = eng.getVariable("b")
    println(c.mkString(" "))

    // evaluate function
    val qfunc: Double = eng.feval("qfuncinv", Array(0.1))
    println(qfunc)

    eng.eval(s"trellis = poly2trellis(7, [171,133])")
    val struct: Struct = eng.getVariable("trellis")
    println(struct.keySet().mkString(" "))
    val result = struct.get("numInputSymbols")
    eng.close()
  }
}
