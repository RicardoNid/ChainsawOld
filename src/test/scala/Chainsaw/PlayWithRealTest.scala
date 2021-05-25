package Chainsaw

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.{Component, SQ, UQ, assert, in, out}
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

class PlayWithRealTest extends AnyFunSuite {

  class RealDUT extends Component {

    // API test
    val UQparams = (0 until 100).map(_ => (DSPRand.nextInt(32), DSPRand.nextInt(32)))
    val UQs = UQparams.map { case (i, f) => QFormatReal(UQ(i + f, f)) }


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

  val DUT = new RealDUT

  test("UQ API"){
    DUT.UQparams.zip(DUT.UQs).foreach{ case (tuple, real) => assert(real.maxExp == tuple._1 && real.minExp == tuple._2,
      s"bad UQ: $tuple lead to ${real.maxExp} & ${real.minExp}")}
  }

}
