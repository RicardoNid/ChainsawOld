package FTN

import Chainsaw._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class TxTest extends AnyFlatSpec with Matchers {

  eng.eval(
    "cd ./matlabWorkspace/FTN326; \n" +
      "load bitsAllFrame; \n" +
      "load codedBitsAllFrame; \n")
  val bits = eng.getVariable[Array[Double]]("bitsAllFrame").map(_.toInt)
  val codedBits = eng.getVariable[Array[Double]]("codedBitsAllFrame").map(_.toInt)
  println(s"coded golden ${codedBits.mkString("")}")

  val testCases = bits.grouped(pFNonIter).toSeq.map(_.mkString(""))
  val forDut = testCases.map(BigInt(_, 2))
  val forMatlab = testCases.map(_.map(_.asDigit.toDouble)).flatten.toArray

  SimConfig.withWave.compile(new Tx(pFNonIter) {
    convencFTN.dataOut.fragment.simPublic()
    convencFTN.dataOut.valid.simPublic()
  }).doSim { dut =>
    import dut.{clockDomain, dataIn, convencFTN}

    val convResult = ArrayBuffer[BigInt]()
    def setMonitor() = fork {
      while (true) {
        if (dut.convencFTN.dataOut.valid.toBoolean) convResult += convencFTN.dataOut.fragment.toBigInt
        clockDomain.waitSampling()
      }
    }

    clockDomain.forkStimulus(2)
    dataIn.last #= false
    dataIn.valid #= false
    clockDomain.waitSampling()

    setMonitor()

    forDut.foreach { testCase =>
      dataIn.valid #= true
      dataIn.fragment #= testCase
      dataIn.last #= (testCase == forDut.last)
      clockDomain.waitSampling()
    }

    dataIn.last #= false
    dataIn.valid #= false
    printlnYellow(s"the total latency of Tx is ${dut.latency}")
    clockDomain.waitSampling(dut.latency + 1)

    println(convResult.map(_.toString(2).padToLeft(pFNonIter, '0')))

    "all the extracted data" should "have correct sizes" in{
      testCases should have size (frameBitsCount / pFNonIter)
      convResult should have size (frameBitsCount / pFNonIter)
    }

  }
}
