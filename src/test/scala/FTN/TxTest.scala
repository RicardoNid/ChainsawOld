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
  //  val modulatedSymbols = eng.getVariable[Array[Double]]("modulatedSymbolsAllFrame").map(new MComplex(_, 0) * params.FFTSize)
  val modulatedSymbols = eng.getVariable[Array[MComplex]]("modulatedSymbolsAllFrame").map(_ * params.FFTSize)
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

    // sim
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
    clockDomain.waitSampling(10)

    "all the extracted data" should "have correct sizes" in {

      import scala.math.ceil
      val cycleCount = ceil(params.BitsPerFramePadded.toDouble / pFNonIter).toInt
      testCases should have size cycleCount
      convResults should have size cycleCount
      interleavedResults should have size cycleCount

      mappedResults should have size params.SymbolsPerChannel * params.CarrierNum * 2
      mappedSymbols should have size params.SymbolsPerChannel * params.CarrierNum

      modulatedResults should have size params.SymbolsPerChannel * params.FFTSize
      modulatedSymbols should have size params.SymbolsPerChannel * params.FFTSize
    }

    it should "be the same as golden" in {
      // conv results processing
      val yourCodedStrings = convResults.map(_.toString(2).padToLeft(pFNonIter * 2, '0')).toArray
      val goldenCodedStrings = codedBits.grouped(pFNonIter * 2).map(_.mkString("")).toArray
      println(s"coded yours  \n${yourCodedStrings.take(4).mkString("\n")}")
      println(s"coded golden \n${goldenCodedStrings.take(4).mkString("\n")}")
      yourCodedStrings shouldBe goldenCodedStrings // compare BigInt by binary string
      printlnGreen(s"convenc module test, passed")

      // interleaved results processing
      val yourInterleavedStrings = interleavedResults.map(_.toString(2).padToLeft(pFNonIter * 2, '0')).toArray
      val goldenInterleavedStrings = interleavedBits.grouped(pFNonIter * 2).map(_.mkString("")).toArray
      println(s"interleaved yours  \n${yourInterleavedStrings.take(4).mkString("\n")}")
      println(s"interleaved golden \n${goldenInterleavedStrings.take(4).mkString("\n")}")
      println(s"qammod input after S2P \n${inputsForMap.take(4).map(_.toString(2).padToLeft(1024, '0')).mkString("\n")}")
      yourInterleavedStrings shouldBe goldenInterleavedStrings
      printlnGreen(s"interleave module test, passed")

      // mapped results processing
      val mappedResultsHalf = mappedResults.grouped(params.FFTSize).toSeq.map(_.take(params.FFTSize / 2)).flatten
      val yourMappedStrings = mappedResults.grouped(params.FFTSize).map(_.map(_.toString(6)).mkString(" ")).toArray
      val goldenMappedStrings = mappedSymbols.grouped(params.FFTSize / 2).map(_.map(_.toString(6)).mkString(" ")).toArray
      println(s"mapped yours  \n${yourMappedStrings.take(4).mkString("\n")}")
      println(s"mapped golden \n${goldenMappedStrings.take(4).mkString("\n")}")
      println(s"for mapped symbol:  \nmax = ${mappedSymbols.map(_.modulus).max}, " +
        s"min = ${mappedSymbols.map(_.modulus).min}")
      val thresholdQAM = 0.05
      assert(mappedResultsHalf.zip(mappedSymbols).forall { case (c0, c1) => c0.sameAs(c1, epsilon = thresholdQAM) })
      printlnGreen(s"QAM module test with threshold = $thresholdQAM, passed")
      val mappedError = mappedResultsHalf.zip(mappedSymbols).map { case (c0, c1) => (c0 - c1).modulus }
      println(s"for mapped symbol errors: \nerror max = ${mappedError.max}, error mean(relative) = ${mappedError.sum / mappedSymbols.map(_.modulus).sum}")

      // modulated results processing
      val yourModulated = modulatedResults.grouped(params.FFTSize).toArray
        .map(_.slice(params.DataCarrierPositions.head - 1, params.DataCarrierPositions.last).toArray).map(_.map(_.real))
      val goldenModulated = modulatedSymbols.grouped(params.FFTSize).toArray
        .map(_.slice(params.DataCarrierPositions.head - 1, params.DataCarrierPositions.last).toArray).map(_.map(_.real))

      val yourModulatedStrings = yourModulated.map(_.map(_.toString.take(6)).mkString(" "))
      val goldenModulatedStrings = goldenModulated.map(_.map(_.toString.take(6)).mkString(" "))

      println(s"modulated yours  \n${yourModulatedStrings.take(4).mkString("\n")}")
      println(s"modulated golden \n${goldenModulatedStrings.take(4).mkString("\n")}")
      println(s"statistic data:  \nmax = ${modulatedSymbols.map(_.real.abs).max}, " +
        s"min = ${modulatedSymbols.map(_.real.abs).min}")

      val yoursFlatten = yourModulated.flatten
      val goldenFlatten = goldenModulated.flatten
      val thresholdIFFT = 0.5
      assert(yoursFlatten.zip(goldenFlatten).forall { case (r0, r1) => (r0 - r1).abs < 0.5 })
      printlnGreen(s"IFFT module test with threshold = $thresholdIFFT, passed")

      val modulatedError = yoursFlatten.zip(goldenFlatten).map { case (r0, r1) => (r0 - r1).abs }
      println(s"for modulated symbol errors: \nerror max = ${modulatedError.max}, error mean(relative) = ${modulatedError.sum / goldenFlatten.map(_.abs).sum}")

      def round64(value: Double) = {
        val ret = if (value > 0 && value < 1) 1
        else if (value > 63) 63
        else if (value < -64) 64
        else scala.math.round(value * 0.5) * 2
        ret.toInt
      }
      val yourQuantized = yourModulated.map(_.map(round64))
      val goldenQuantized = goldenModulated.map(_.map(round64))
      val yourQuantizedStrings = yourQuantized.map(_.map(_.toString.padTo(4, ' ')).mkString(" "))
      val goldenQuantizedStrings = goldenQuantized.map(_.map(_.toString.padTo(4, ' ')).mkString(" "))

      println(s"quantized yours  \n${yourQuantizedStrings.take(4).mkString("\n")}")
      println(s"quantized golden \n${goldenQuantizedStrings.take(4).mkString("\n")}")
      val yourQuantizedF = yourQuantized.flatten
      val goldenQuantizedF = goldenQuantized.flatten
      val errorCount = yourQuantizedF.zip(goldenQuantizedF).filter{ case (i0, i1) => i0 != i1 }.size
      val errorMax = yourQuantizedF.zip(goldenQuantizedF).map{ case (i0, i1) => (i0- i1).abs}.max
      println(s"error ratio = ${errorCount.toDouble / yourQuantizedF.size}, error max = $errorMax")
      printlnGreen(s"quantized result test passed with error count = ${errorCount * 2} / ${yourQuantizedF.size * 2}")

      // details on mapped symbols
      println(mappedResultsHalf.zip(mappedSymbols).zipWithIndex
        .filter { case ((c0, c1), i) => (c0 - c1).modulus > 0.5 }
        .map { case ((c0, c1), i) => s"i = $i, bitAllocated = ${params.bitAlloc(i % 256)}, powAllocated = ${params.powAlloc(i % 256)}, yours = $c0, golden = $c1" }
        .mkString("\n"))

      // details on modulated symbols
      val errors = yoursFlatten.zip(goldenFlatten).zipWithIndex
        .filter { case ((r0, r1), i) => (r0 - r1).abs > 1.0 }
        .map { case ((r0, r1), i) => (r0, r1, i) }

      val errorsForOne = modulatedResults.take(512).map(_.real).zip(modulatedSymbols.take(512).map(_.real))
        .map { case (r0, r1) => (r0 - r1).abs }

      eng.putVariable("error", errorsForOne.toArray)
      eng.eval("plot(error)")

      println(errors.map { case (r0, r1, i) => s"i = $i, yours = $r0, golden = $r1, epsilon = ${(r0 - r1).abs}" }.mkString("\n"))
      println(errors.length)
    }
  }
}
