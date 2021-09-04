package FTN

import Chainsaw._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import spinal.core.sim._
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._

import matlabIO._

import scala.collection.mutable.ArrayBuffer

class TxTest extends AnyFlatSpec with Matchers {

  eng.eval(
    "cd ./matlabWorkspace/FTN326; \n" +
      "load bitsAllFrame; \n" +
      "load codedBitsAllFrame; \n" +
      "load interleavedBitsAllFrame; " +
      "load mappedSymbolsAllFrame; \n"
  )

  println(s"FFTSize = ${params.FFTSize}")

  val bits = eng.getVariable[Array[Double]]("bitsAllFrame").map(_.toInt)
  val codedBits = eng.getVariable[Array[Double]]("codedBitsAllFrame").map(_.toInt)
  val interleavedBits = eng.getVariable[Array[Double]]("interleavedBitsAllFrame").map(_.toInt)
  val mappedSymbols = eng.getVariable[Array[MComplex]]("mappedSymbolsAllFrame")
  //  println(s"mapped symbols")
  //  println(mappedSymbols.mkString(" "))

  val testCases = bits.grouped(pFNonIter).toSeq.map(_.mkString(""))
  val forDut = testCases.map(BigInt(_, 2))

  SimConfig.withWave.compile(new Tx {
    // exposing signals for debugging
    convencFTN.dataOut.simPublic()
    interleaverFTN.dataOut.simPublic()
    qammodFTN.dataOut.simPublic()
    qammodFTN.core.dataIn.simPublic()
    qammodFTN.remapped.simPublic()
  }).doSim { dut =>
    import dut.{clockDomain, dataIn, convencFTN}

    clockDomain.forkStimulus(2)
    dataIn.last #= false
    dataIn.valid #= false
    clockDomain.waitSampling()

    // set monitors for modules
    def setMonitor[T <: BitVector](trigger: Bool, target: T, Container: ArrayBuffer[BigInt]) = fork {
      while (true) {
        if (trigger.toBoolean) Container += target.toBigInt
        clockDomain.waitSampling()
      }
    }

    def setComplexMonitor(trigger: Bool, target: Vec[ComplexNumber], Container: ArrayBuffer[MComplex]) = fork {
      while (true) {
        if (trigger.toBoolean) Container ++= target.map(_.toComplex)
        clockDomain.waitSampling()
      }
    }

    val convResults, interleavedResults, inputsForMap = ArrayBuffer[BigInt]()
    val mappedResults = ArrayBuffer[MComplex]()

    setMonitor(dut.convencFTN.dataOut.valid, convencFTN.dataOut.fragment, convResults)
    setMonitor(dut.interleaverFTN.dataOut.valid, dut.interleaverFTN.dataOut.fragment, interleavedResults)
    setMonitor(dut.qammodFTN.core.dataIn.valid, dut.qammodFTN.remapped, inputsForMap)
    setComplexMonitor(dut.qammodFTN.dataOut.valid, dut.qammodFTN.dataOut.fragment, mappedResults)

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

    // conv results processing
    val yourCodedStrings = convResults.map(_.toString(2).padToLeft(pFNonIter * 2, '0')).toArray
    val goldenCodedStrings = codedBits.grouped(pFNonIter * 2).map(_.mkString("")).toArray
    println(s"coded yours  \n${yourCodedStrings.take(4).mkString("\n")}")
    println(s"coded golden \n${goldenCodedStrings.take(4).mkString("\n")}")

    // interleaved results processing
    val yourInterleavedStrings = interleavedResults.map(_.toString(2).padToLeft(pFNonIter * 2, '0')).toArray
    val goldenInterleavedStrings = interleavedBits.grouped(pFNonIter * 2).map(_.mkString("")).toArray
    println(s"interleaved yours  \n${yourInterleavedStrings.take(4).mkString("\n")}")
    println(s"interleaved golden \n${goldenInterleavedStrings.take(4).mkString("\n")}")

    // mapped results processing
    val yourMappedResults = mappedResults.grouped(params.FFTSize / 2).map(_.map(_.toString(6)).mkString(" ")).toArray
    val goldenMappedResults = mappedSymbols.grouped(params.FFTSize / 2).map(_.map(_.toString(6)).mkString(" ")).toArray
    println(s"mapped yours  \n${yourMappedResults.take(4).mkString("\n")}")
    println(s"mapped golden \n${goldenMappedResults.take(4).mkString("\n")}")

    println(s"map input     \n${inputsForMap.take(4).map(_.toString(2).padToLeft(1024, '0')).mkString("\n")}")

    "all the extracted data" should "have correct sizes" in {

      import scala.math.ceil
      val cycleCount = ceil(params.BitsPerFramePadded.toDouble / pFNonIter).toInt
      testCases should have size cycleCount
      convResults should have size cycleCount
      interleavedResults should have size cycleCount

//      mappedResults should have size params.SymbolsPerChannel * params.CarrierNum
    }

    it should "be the same as golden" in {
      yourCodedStrings shouldBe goldenCodedStrings // compare BigInt by binary string
      yourInterleavedStrings shouldBe goldenInterleavedStrings
//      mappedResults.toArray shouldBe mappedSymbols
//      assert(mappedResults.zip(mappedSymbols).forall{ case (yours, golden) => yours.sameAs(golden, epsilon = 1.0)})
    }

  }
}
