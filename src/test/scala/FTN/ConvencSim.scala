package FTN

import Chainsaw.{DSPSimTiming, eng}
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import spinal.core.{Bool, Vec}

import scala.collection.mutable.ListBuffer

class ConvencSim(config: ConvencConfig) extends ConvencDUT(config) with DSPSimTiming[Bool, Vec[Bool], Array[Double], Array[Double]] {

  override def poke(testCase: Array[Double], input: Bool): Unit = {
    testCase.init.foreach { bit =>
      input #= (bit == 1.0)
      clockDomain.waitSampling()
    }
    input #= (testCase.last == 1.0)
  }

  override def peek(output: Vec[Bool]): Array[Double] = {
    val buffer = ListBuffer[Boolean]()
    (0 until timing.outputInterval).init.foreach { _ =>
      val temp = output.map(_.toBoolean)
      buffer ++= output.map(_.toBoolean)
      clockDomain.waitSampling()
    }
    buffer ++= output.map(_.toBoolean)
    eng.putVariable("convCodedBitsByHardware", buffer.toArray.map(if (_) 1.0 else 0.0))
    eng.eval("save './data/convCodedBitsByHardware' convCodedBitsByHardware;")
    buffer.toArray.map(if (_) 1.0 else 0.0)
  }

  override def referenceModel(testCase: Array[Double]): Array[Double] = convenc.referenceModel(testCase)

  override def isValid(refResult: Array[Double], dutResult: Array[Double]): Boolean = refResult.zip(dutResult).forall(pair => pair._1 == pair._2)

  override def messageWhenInvalid(testCase: Array[Double], refResult: Array[Double], dutResult: Array[Double]): String = {
    val rowCount = config.gens.length
    val refOutput = (0 until rowCount).map(i => refResult.zipWithIndex.filter(_._2 % rowCount == i).map(_._1).map(_.toInt).mkString(" ")).mkString("\n")
    val dutOutput = (0 until rowCount).map(i => dutResult.zipWithIndex.filter(_._2 % rowCount == i).map(_._1).map(_.toInt).mkString(" ")).mkString("\n")
    s"testCase: ${testCase.map(_.toInt).mkString(" ")}\ngolden:\n${refOutput}\nyours:\n${dutOutput}\n"
  }

  override def messageWhenValid(testCase: Array[Double], refResult: Array[Double], dutResult: Array[Double]): String = {
    messageWhenInvalid(testCase, refResult, dutResult)
  }
}

class testConvenc extends AnyFunSuite {
  test("testConvenc") {
    SimConfig.withWave.compile(new ConvencSim(ConvencConfig(7, Array(171, 133)))).doSim { dut =>
      dut.sim()
      // the actual case of FTN
      eng.eval("cd /home/ltr/IdeaProjects/Chainsaw/src/main/scala/FTN/Matlab")
      eng.eval("load './data/bits'")
      val bits = eng.getVariable("bits").asInstanceOf[Array[Double]]
      dut.insertTestCase(bits)
      //      dut.insertTestCase(Array(false, false, false, false, false, true, true, true, true, true) :+ false)
      //      dut.insertTestCase(Array.fill(10)(false) :+ false)
      //      dut.insertTestCase(Array(true, false, true, false, true, false, true, false, true, false) :+ false)
      val report = dut.simDone()
      println(report.validLog)
    }
  }
}