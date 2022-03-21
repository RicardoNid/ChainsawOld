package Chainsaw.examples

import Chainsaw._
import Chainsaw.matlabIO._
import com.mathworks.engine.MatlabEngine

import java.lang.Thread.sleep
import java.time.{Duration, Instant}

object HelloMatlab {
  @throws[Exception]
  def main(args: Array[String]): Unit = {
    // SYNC/ASYNC
    def testDuration(func: => Unit): Unit = {
      val start = Instant.now()
      func
      val end      = Instant.now()
      val duration = Duration.between(start, end)
      println(duration.toMillis)
    }
    testDuration(MatlabEngine.startMatlab()) // start the engine takes about 3000 milliseconds
    testDuration(MatlabEngine.startMatlabAsync()) // the Future[MatlabEngine] object is returned instantly, takes 2 milliseconds
    testDuration { // the whole process still takes 3000 milliseconds
      val eng = MatlabEngine.startMatlabAsync()
      sleep(3000) // but you can do something else here without extra cost
      eng.get()
    }

    // feval/eval
    val eng = AsyncEng.get()
    println(eng.feval[Double]("sin", Array(0.05)))
    //    println(eng.feval[Double]("sin", 0.05)) // wrong, 0.05 is not an array

    eng.putVariable("input", 0.05)
    eng.eval("sin(input)")
    println(eng.getVariable[Double]("ans"))

    // about array size
    val anotherComplex = Array(BComplex(1, 2)) //
    eng.putVariable("anotherComplex", anotherComplex)
    val ret = eng.getVariable[BComplex]("anotherComplex")
    println(ret.getClass)
    val retArray =
      try {
        eng.getVariable[Array[BComplex]]("anotherComplex")
      } catch {
        case _ => println("failed")
      }

    // jagged array
    val jagged = Array(Array(1, 2), Array(1, 2, 3))
    eng.putVariable("jagged", jagged)
    println(eng.getVariable[Array[Array[Int]]]("jagged").map(_.mkString(" ")).mkString("\n"))
    println(eng.getVariable[BComplex]("complex"))

  }
}
