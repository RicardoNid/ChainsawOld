import com.mathworks.engine._

object HelloMatlab {
  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val eng = MatlabEngine.startMatlab
    val a = Array(0.1,0.2,0.3)
    val roots = eng.feval("qfuncinv", a).asInstanceOf[Array[Double]]
    for (e <- roots) {
      System.out.println(e)
    }
    eng.close()
  }
}