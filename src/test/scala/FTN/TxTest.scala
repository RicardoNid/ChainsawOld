package FTN

import Chainsaw._
import Chainsaw.matlabIO._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

class TxTest extends AnyFlatSpec with Matchers {

  eng.eval(
    "cd ./matlabWorkspace/FTN326; \n" +
      "load bitsAllFrame; \n" +
      "load codedBitsAllFrame; \n" +
      "load interleavedBitsAllFrame; " +
      "load mappedSymbolsAllFrame; \n" +
      "load modulatedSymbolsAllFrame; \n"
  )

  println(s"FFTSize = ${params.FFTSize}")

  val bits = eng.getVariable[Array[Double]]("bitsAllFrame").map(_.toInt)
  val codedBits = eng.getVariable[Array[Double]]("codedBitsAllFrame").map(_.toInt)
  val interleavedBits = eng.getVariable[Array[Double]]("interleavedBitsAllFrame").map(_.toInt)
  val mappedSymbols = eng.getVariable[Array[MComplex]]("mappedSymbolsAllFrame")
  val modulatedSymbols = eng.getVariable[Array[Double]]("modulatedSymbolsAllFrame").map(new MComplex(_, 0))
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
    qammodFTN.hermitianExpanded.simPublic()
    IfftFTN.dataOut.simPublic()
  }).doSim { dut =>
    import dut.{clockDomain, convencFTN, dataIn}

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
    val mappedResults, modulatedResults = ArrayBuffer[MComplex]()

    setMonitor(dut.convencFTN.dataOut.valid, convencFTN.dataOut.fragment, convResults)
    setMonitor(dut.interleaverFTN.dataOut.valid, dut.interleaverFTN.dataOut.fragment, interleavedResults)
    setMonitor(dut.qammodFTN.core.dataIn.valid, dut.qammodFTN.remapped, inputsForMap)
    setComplexMonitor(dut.qammodFTN.dataOut.valid, dut.qammodFTN.dataOut.fragment, mappedResults)
    setComplexMonitor(dut.IfftFTN.dataOut.valid, dut.IfftFTN.dataOut.fragment, modulatedResults)

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

    println(s"qammod input after S2P \n${inputsForMap.take(4).map(_.toString(2).padToLeft(1024, '0')).mkString("\n")}")

    // mapped results processing
    val mappedResultsHalf = mappedResults.grouped(params.FFTSize).toSeq.map(_.take(params.FFTSize / 2)).flatten
    val yourMappedStrings = mappedResults.grouped(params.FFTSize).map(_.map(_.toString(6)).mkString(" ")).toArray
    //    val yourMappedStrings = mappedResultsHalf.grouped(params.FFTSize / 2).map(_.map(_.toString(6)).mkString(" ")).toArray
    val goldenMappedStrings = mappedSymbols.grouped(params.FFTSize / 2).map(_.map(_.toString(6)).mkString(" ")).toArray
    println(s"mapped yours  \n${yourMappedStrings.take(4).mkString("\n")}")
    println(s"mapped golden \n${goldenMappedStrings.take(4).mkString("\n")}")

    //
    val yourModulatedStrings = modulatedResults.grouped(params.FFTSize).map(_.map(_.toString(6)).mkString(" ")).toArray
    val goldenModulatedStrings = modulatedSymbols.grouped(params.FFTSize).map(_.map(_.toString(6)).mkString(" ")).toArray
    println(s"modulated yours  \n${yourModulatedStrings.take(4).mkString("\n")}")
    println(s"modulated golden \n${goldenModulatedStrings.take(4).mkString("\n")}")

    "all the extracted data" should "have correct sizes" in {

      import scala.math.ceil
      val cycleCount = ceil(params.BitsPerFramePadded.toDouble / pFNonIter).toInt
      testCases should have size cycleCount
      convResults should have size cycleCount
      interleavedResults should have size cycleCount

      mappedResults should have size params.SymbolsPerChannel * params.CarrierNum * 2
      mappedSymbols should have size params.SymbolsPerChannel * params.CarrierNum
    }

    it should "be the same as golden" in {
      yourCodedStrings shouldBe goldenCodedStrings // compare BigInt by binary string
      yourInterleavedStrings shouldBe goldenInterleavedStrings
      assert(mappedResultsHalf.zip(mappedSymbols).forall { case (c0, c1) => c0.sameAs(c1, epsilon = 0.1) })

      printlnRed(DSP.FFT.Refs.IFFT(mappedResults.take(512).toArray).mkString(" "))

      //      // TODO: following code showed that custom symbols(QAM8) are not implemented correctly
      //      println(mappedResultsHalf.zip(mappedSymbols).zipWithIndex
      //        .filter { case ((c0, c1), i) => (c0 - c1).modulus > 0.5 }
      //        .map { case ((c0, c1), i) => s"i = $i, bitAllocated = ${params.bitAlloc(i % 256)}, powAllocated = ${params.powAlloc(i % 256)}, yours = $c0, golden = $c1" }
      //        .mkString("\n"))
    }
  }
}
