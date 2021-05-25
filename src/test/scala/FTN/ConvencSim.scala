package FTN

import Chainsaw.{ChainsawDebug, DSPSimTiming, eng}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core.{Bool, Vec}

import scala.collection.mutable.ListBuffer

class ConvencSim(config: ConvencConfig) extends ConvencDUT(config) with DSPSimTiming[Bool, Vec[Bool], Array[Boolean], Array[Boolean]] {

  override def poke(testCase: Array[Boolean], input: Bool): Unit = {
    testCase.init.foreach { bit =>
      input #= bit
      clockDomain.waitSampling()
    }
    input #= testCase.last
  }

  override def peek(output: Vec[Bool]): Array[Boolean] = {
    val buffer = ListBuffer[Boolean]()
    (0 until timing.outputInterval).init.foreach { _ =>
      val temp = output.map(_.toBoolean)
      buffer ++= output.map(_.toBoolean)
      clockDomain.waitSampling()
    }
    buffer ++= output.map(_.toBoolean)
    buffer.toArray
  }

  override def referenceModel(testCase: Array[Boolean]): Array[Boolean] = convenc.referenceModel(testCase)

  override def isValid(refResult: Array[Boolean], dutResult: Array[Boolean]): Boolean = refResult.zip(dutResult).forall(pair => pair._1 == pair._2)

  override def messageWhenInvalid(testCase: Array[Boolean], refResult: Array[Boolean], dutResult: Array[Boolean]): String = {
    val rowCount = config.gens.length
    val refOutput = (0 until rowCount).map(i => refResult.zipWithIndex.filter(_._2 % rowCount == i).map(_._1).map(if (_) 1 else 0).mkString(" ")).mkString("\n")
    val dutOutput = (0 until rowCount).map(i => dutResult.zipWithIndex.filter(_._2 % rowCount == i).map(_._1).map(if (_) 1 else 0).mkString(" ")).mkString("\n")
    s"testCase: ${testCase.map(if (_) 1 else 0).mkString(" ")}\ngolden:\n${refOutput}\nyours:\n${dutOutput}\n"
  }

  override def messageWhenValid(testCase: Array[Boolean], refResult: Array[Boolean], dutResult: Array[Boolean]): String = {
    messageWhenInvalid(testCase, refResult, dutResult)
  }
}

class testConvenc extends AnyFunSuite {

  test("testConvenc") {
//    SimConfig.withWave.compile(new ConvencSim(ConvencConfig(7, Array(145, 133)))).doSim { dut =>
//      dut.sim()
//      dut.insertTestCase(Array(false, false, false, false, false, true, true, true, true, true) :+ false)
//      dut.insertTestCase(Array.fill(10)(false) :+ false)
//      dut.insertTestCase(Array(true, false, true, false, true, false, true, false, true, false) :+ false)
//      val report = dut.simDone()
//    }
//    eng.close()
    Test.main(Array(""))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new ConvencSim(ConvencConfig(7, Array(145, 133)))).doSim { dut =>
      dut.sim()
      dut.insertTestCase(Array(false, false, false, false, false, true, true, true, true, true) :+ false)
      dut.insertTestCase(Array.fill(10)(false) :+ false)
      dut.insertTestCase(Array(true, false, true, false, true, false, true, false, true, false) :+ false)
      val report = dut.simDone()
    }
    eng.close()
  }
}
