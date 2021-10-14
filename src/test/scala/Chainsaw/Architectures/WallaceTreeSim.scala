package Chainsaw.Architectures

import Chainsaw.{Real, _}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

// TODO: sign extension
class WallaceTreeDUT extends DSPDUTTiming[Vec[Real], Real] {
  override val input = in Vec(Real(1, 2.5, -5 exp), 7)
  val wallaceTree = WallaceTree(input)
  override val output = out(wallaceTree.implicitValue)
  override val timing = wallaceTree.getTimingInfo
}

class WallaceTreeSim() extends WallaceTreeDUT with DSPSimTiming[Vec[Real], Real, Seq[Double], Double] {

  override def poke(testCase: Seq[Double], input: Vec[Real]): Unit = input #= testCase

  override def peek(output: Real): Double = output.toDouble

  override def referenceModel(testCase: Seq[Double]) = testCase.sum

  override def isValid(refResult: Double, dutResult: Double) = refResult == dutResult

  override def messageWhenInvalid(testCase: Seq[Double], refResult: Double, dutResult: Double) =
    s"testCase: $testCase, golden: $refResult, yours: $dutResult "

  override def messageWhenValid(testCase: Seq[Double], refResult: Double, dutResult: Double) =
    s"testCase: $testCase, golden: $refResult, yours: $dutResult "
}

class testWallaceTreeTree extends AnyFunSuite {
  test("testWallaceTree") {
    SimConfig.withWave.compile(new WallaceTreeSim).doSim { dut =>
      dut.sim()
      //      (0 until 10).foreach(_ => dut.insertTestCase(dut.input.payload.randomValue))
      (0 until 10).foreach(_ => dut.insertTestCase((0 until 7).map(_ => 2.5)))
      val report = dut.simDone()
      printlnGreen(report.validLog.mkString("\n"))
    }
  }
}