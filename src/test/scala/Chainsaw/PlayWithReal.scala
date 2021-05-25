package Chainsaw

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The thorough test of Real type
 *
 */
class PlayWithReal extends Component {

  // API test
  val UQparams = (0 until 100).map(_ => (DSPRand.nextInt(32), DSPRand.nextInt(32)))
  val UQs = UQparams.map { case (i, f) => QFormatReal(UQ(i + f, f)) }
  UQparams.zip(UQs).foreach{ case (tuple, real) => assert(real.maxExp == tuple._1 && real.minExp == tuple._2,
    s"bad UQ: $tuple lead to ${real.maxExp} & ${real.minExp}")}

  val SQparams = (0 until 100).map(_ => (DSPRand.nextInt(32), DSPRand.nextInt(32)))
  val SQs = SQparams.map { case (i, f) => QFormatReal(SQ(i + f + 1, f)) }
  SQparams.zip(SQs).foreach{ case (tuple, real) => assert(real.maxExp == tuple._1 && real.minExp == tuple._2,
    s"bad SQ: $tuple lead to ${real.maxExp} & ${real.minExp}")}

  val randomRanges = (0 until 1000).map { i =>
    val lower = DSPRand.nextDouble() * 10 + 0.5
    RealInfo(lower, lower + DSPRand.nextDouble() * 10)
  }
  val randomInputs = randomRanges.map(info => Real(info, -4 exp))
  // more or less on LSB, check "equal to" or "close to" in the simulation
  val randomOutputs = randomRanges.map(info => Real(info + 1, (DSPRand.nextInt(2) - 1) - 4 exp))
  randomInputs.zip(randomOutputs).foreach { case (real, real1) => real1 := real.truncated }
  in(randomInputs: _*)
  out(randomOutputs: _*)

  //  val a0 = SReal(4 exp, -4 exp)
  //  val a1 = SReal(4 exp, -1 exp)
  //  val a2 = SReal(1 exp, -4 exp)
  //  val b = SReal(2 exp, -2 exp)
  //  val b = SReal(2 exp, -2 exp)

  val a0 = QFormatReal(SQ(9, 4))
  println(a0.realInfo)
  val a1 = QFormatReal(SQ(6, 1))
  val a2 = QFormatReal(SQ(6, 1))
  val b = QFormatReal(SQ(5, 2))


  val randomRangesForAddition = (0 until 20).map { i =>
    val lower = DSPRand.nextDouble() * 10
    RealInfo(lower, lower + DSPRand.nextDouble() * 10)
  }
  val randomInputsForAddition = randomRangesForAddition.map(info => Real(info, -DSPRand.nextInt(5) exp))
  val randomOutputsForAddtion = (0 until randomInputsForAddition.length / 2).map(i =>
    randomInputsForAddition(2 * i) + randomInputsForAddition(2 * i + 1))

  val tangent0 = a0 + a1
  val tangent1 = a0 + a2
  val contains = a0 + b

  // TODO: implement this part in Real
  //  val c = SReal(7 exp, 4 exp)
  //  val d = SReal(-4 exp, -7 exp)
  //  val overlap0 = a0 + c
  //  val overlap1 = a0 + d
  //  val separated0 = b + c
  //  val separated1 = b + d
  //  in(c, d)
  //  out(overlap0, overlap1, seperated0, seperated1)

  in(a0, a1, a2, b)
  in(randomInputsForAddition: _*)
  out(contains, tangent0, tangent1)
  out(randomOutputsForAddtion: _*)

  //  val constantsForAddtion = (0 until 100).map(_ => DSPRand.nextDouble() * 10 - 5)
  //  val outputsOfConstantAddition = constantsForAddtion.map(constant => a0 + constant.roundAsScala(a0.ulp))
  //  out(outputsOfConstantAddition: _*)

  //  val f = SReal(3 exp, -3 exp)
  //  val g = SReal(RealRange(-0.3, 0.3, 0.1))
  //  val h = SReal(RealRange(-0.3, 0.3, 0.1))
  //  val i = SReal(RealRange(0.3, 0.8, 0.1))
  //  val j = SReal(RealRange(-0.3, -0.8, 0.1))

  val f = QFormatReal(SQ(7, 3))
  val g = Real(-0.3, 0.3, 0.1)
  val h = Real(-0.3, 0.3, 0.1)
  val i = Real(0.3, 0.8, 0.1)
  val j = Real(-0.8, -0.3, 0.1)

  val mul = f * f
  val precisemul0 = g * h
  val precisemul1 = g * i
  val precisemul2 = i * j

  in(f, g, h, i, j)
  out(mul, precisemul0, precisemul1, precisemul2)

  val r0 = Real(-1, 1, -3 exp)
  val r1 = Real(-1, 1, -3 exp)
  val r0mult1 = r0 * r1
  val truncated = Real(new RealInfo(new AffineForm(0, Map("z" -> 1.0)), 0.0), -5 exp)
  truncated := r0mult1.truncated
  in(r0, r1)
  out(r0mult1, truncated)

  println(r0mult1.realInfo)
  println(r0mult1.minExp)
  println(r0mult1.realInfo.interval)
  println(truncated.realInfo)

}


class testPlayWithReal extends AnyFunSuite {

  private def rangeToWidthTest(inputs: IndexedSeq[Real], outputs: IndexedSeq[Real]) = {
    inputs.zip(outputs).foreach { case (input, output) =>
      input.allValues.foreach { value =>
        try {
          input #= value
        }
        catch {
          case _: AssertionError => println(s"range: ${input.realInfo}, value: $value")
          case _ =>
        }
        sleep(1)
        if (output.minExp <= input.minExp && output.toDouble != value || // equal to
          !(output ~= value)) // close to
          println(s"value: $value, output: ${output.toDouble}, ${output.realInfo}")
      }
    }
  }

  private def traversalAdditionTest(a: Real, b: Real, c: Real) = {
    println(s"${a.realInfo}, ${b.realInfo}")
    println(s"${a.allValues.length * b.allValues.length} testCases to be tested")
    for (va <- a.allValues; vb <- b.allValues) {
      a #= va
      b #= vb
      sleep(1)
      assert(c.toDouble == a.toDouble + b.toDouble, s"${c.toDouble} != ${a.toDouble} + ${b.toDouble}")
    }
  }

  private def traversalAdditionTest(a: Real, b: Double, c: Real) = {
    println(s"${a.allValues.length} testCases to be tested")
    for (va <- a.allValues) {
      a #= va
      sleep(1)

      if (b.abs > a.ulp)
        assert(c.toDouble == a.toDouble + b.roundAsScala(a.ulp),
          s"ulp: ${a.ulp} ${c.toDouble} != ${a.toDouble} + ${b.roundAsScala(a.ulp)}")
    }
  }


  private def traversalMultiplicationTest(a: Real, b: Real, c: Real) = {
    println(s"${a.allValues.length * b.allValues.length} testCases to be tested")
    for (va <- a.allValues; vb <- b.allValues) { // TODO: better API
      a #= va
      b #= vb
      sleep(1)
      assert(c ~= a.toDouble * b.toDouble,
        s"${c.toDouble} != ${a.toDouble} * ${b.toDouble}, " +
          s" \na: ${a.realInfo}, \nb: ${b.realInfo}, \nc: ${c.realInfo}")
    }
  }



  test("testPlayWithRealElaboration") {
    SpinalConfig().generateSystemVerilog(new PlayWithReal)
  }

  test("testPlayWithRealSim") {
    SimConfig.compile(new PlayWithReal).doSim {
      dut =>
        import dut._

        println("start range-width test")
        rangeToWidthTest(randomInputs, randomOutputs)
        println(Console.GREEN)
        println("RANGE-WIDTH TEST PASSED !")
        println(Console.BLACK)

        // TODO: add these tests
        //                    (a0, c, overlap0),
        //                    (a0, d, overlap1),
        //                    (b, c, seperated0),
        //                    (b, d, seperated1)
        val additionTests = Array(
          (a0, a1, tangent0),
          (a0, a2, tangent1),
          (a0, b, contains)) ++ (0 until randomInputsForAddition.length / 2).map(i =>
          (randomInputsForAddition(2 * i), randomInputsForAddition(2 * i + 1),
            randomOutputsForAddtion(i)))

        additionTests.zipWithIndex.foreach { case (tuple, i) =>
          println(s"start addition test $i")
          traversalAdditionTest(tuple._1, tuple._2, tuple._3)
        }
        println(Console.GREEN)
        println("ADDITION TEST PASSED !")
        println(Console.BLACK)

        //        println(s"start constant addition test")
        //        constantsForAddtion.zip(outputsOfConstantAddition).foreach { case (d, real) =>
        //          traversalAdditionTest(a0, d, real)
        //        }

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

