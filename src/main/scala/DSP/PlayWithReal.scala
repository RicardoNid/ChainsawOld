package DSP

import spinal.core._
import spinal.core.sim._

/** The thorough test of Real type
 *
 */
class PlayWithReal extends Component {

  val randomRanges = (0 until 1000).map(_ => RealRange(DSPRand.nextDouble() * 10, DSPRand.nextDouble() * 10, DSPRand.nextDouble()))
  val randomInputs = randomRanges.map(SReal(_))
  val randomOutputs = randomRanges.map(SReal(_))
  randomInputs.zip(randomOutputs).foreach { case (real, real1) => real1 := real }
  in(randomInputs: _*)
  out(randomOutputs: _*)

  val a0 = SReal(5 exp, -5 exp)
  val a1 = SReal(5 exp, -1 exp)
  val a2 = SReal(1 exp, -5 exp)
  val b = SReal(2 exp, -2 exp)
  val c = SReal(7 exp, 4 exp)
  val d = SReal(-4 exp, -7 exp)
  val randomRangesForAddition = (0 until 10).map(_ => RealRange(DSPRand.nextDouble() * 10, DSPRand.nextDouble() * 10, DSPRand.nextDouble()))
  val randomInputsForAddition = randomRangesForAddition.map(SReal(_))
  //  val randomOutputsForAddtion =

  val tangent0 = a0 + a1
  val tangent1 = a0 + a2
  val contains = a0 + b
  val overlap0 = a0 + c
  val overlap1 = a0 + d
  val seperated0 = b + c
  val seperated1 = b + d

  in(a0, a1, a2, b, c, d)
  out(contains, tangent0, tangent1, overlap0, overlap1, seperated0, seperated1)

  val f = SReal(3 exp, -3 exp)
  val g = SReal(RealRange(-0.3, 0.3, 0.1))
  val h = SReal(RealRange(-0.3, 0.3, 0.1))
  val i = SReal(RealRange(0.3, 0.8, 0.1))
  val j = SReal(RealRange(-0.3, -0.8, 0.1))

  val mul = f * f
  val precisemul0 = g * h
  val precisemul1 = g * i
  val precisemul2 = i * j

  in(f, g, h, i, j)
  out(mul, precisemul0, precisemul1, precisemul2)
}


object PlayWithReal {

  private def rangeToWidthTest(inputs: IndexedSeq[SReal], outputs: IndexedSeq[SReal]) = {
    inputs.zip(outputs).foreach { case (input, output) =>
      input.numericInfo.range.allValues.foreach { value =>
        try {
          input #= value
        }
        catch {
          case _: AssertionError => println(s"range: ${input.numericInfo}, value: $value")
          case _ =>
        }
        sleep(1)
        if (output.toDouble != value) println(s"value: $value, output: $output")
      }
    }
  }

  private def traversalAdditionTest(a: SReal, b: SReal, c: SReal) = {
    println(s"${a.numericInfo.range.allValues.length * b.numericInfo.range.allValues.length} testCases to be tested")
    for (va <- a.numericInfo.range.allValues; vb <- b.numericInfo.range.allValues) { // TODO: better API
      a #= va
      b #= vb
      sleep(1)
      assert(c.toDouble == a.toDouble + b.toDouble, s"${c.toDouble} != ${a.toDouble} + ${b.toDouble}")
    }
  }

  private def traversalMultiplicationTest(a: SReal, b: SReal, c: SReal) = {
    println(s"${a.numericInfo.range.allValues.length * b.numericInfo.range.allValues.length} testCases to be tested")
    for (va <- a.numericInfo.range.allValues; vb <- b.numericInfo.range.allValues) { // TODO: better API
      a #= va
      b #= vb
      sleep(1)
      assert(c ~= a.toDouble * b.toDouble,
        s"${c.toDouble} != ${a.toDouble} * ${b.toDouble}, " +
          s" \na: ${a.numericInfo}, \nb: ${b.numericInfo}, \nc: ${c.numericInfo}")
    }
  }

  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new PlayWithFix)
    SimConfig.compile(new PlayWithReal).doSim {
      dut =>
        import dut._

        println("start range-width test")
        rangeToWidthTest(randomInputs, randomOutputs)
        println(Console.GREEN)
        println("RANGE-WIDTH TEST PASSED !")
        println(Console.BLACK)

        //        val additionTests = Array(
        //          (a0, a1, tangent0),
        //          (a0, a2, tangent1),
        //          (a0, b, contains),
        //          (a0, c, overlap0),
        //          (a0, d, overlap1),
        //          (b, c, seperated0),
        //          (b, d, seperated1)
        //        )
        //        additionTests.zipWithIndex.foreach { case (tuple, i) =>
        //          println(s"start addition test $i")
        //          traversalAdditionTest(tuple._1, tuple._2, tuple._3)
        //        }
        //        println(Console.GREEN)
        //        println("ADDITION TEST PASSED !")
        //        println(Console.BLACK)

        println(s"start multiplication test")
        traversalMultiplicationTest(f, f, mul)
        traversalMultiplicationTest(g, h, precisemul0)
        traversalMultiplicationTest(g, i, precisemul1)
        traversalMultiplicationTest(i, j, precisemul2)
        println(Console.GREEN)
        println("MULTIPLICATION TEST PASSED !")
        println(Console.BLACK)
    }
  }
}

