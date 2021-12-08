package Chainsaw.comm

import Chainsaw.comm.qam.{QAMMod, Refs}
import Chainsaw._
import matlabIO._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._

import scala.math.sqrt

class QAMModTest extends AnyFunSuite {
  // TODO: design tests for more different situations
  test("test qammod without bitAlloc and powAlloc"){
    val pF = 256
    val bitPerSymbol = 4

    val bitAlloc = Seq.fill(pF)(bitPerSymbol)
    val powAlloc = Seq.fill(pF)(1.0)
    val symbolType = HardType(ComplexNumber(1, -14))

    SimConfig.withWave.compile(new QAMMod(bitAlloc, powAlloc, symbolType)).doSim{dut =>
      import dut.{clockDomain, dataIn, dataOut}
      clockDomain.forkStimulus(2)
      dataIn.valid #= false
      clockDomain.waitSampling()

      val testCase = bitAlloc.map(bitAllocated => ChainsawRand.nextInt(1 << bitAllocated)).toArray
      val testBits = BigInt(testCase.zip(bitAlloc).map{ case (value, bitAllocated) => value.toBinaryString.padToLeft(bitAllocated, '0')}.mkString(""), 2)

      dataIn.payload #= testBits
      dataIn.valid #= true
      clockDomain.waitSampling(2)

      val golden = Refs.qammod(testCase, bitPerSymbol).map(_ / sqrt(10))
      val yours = dataOut.payload.map(_.toComplex)
      println(yours.mkString(" "))
      println(golden.mkString(" "))
      assert(golden.zip(yours).forall{ case (c0, c1) => c0.sameAs(c1, 0.01) })
    }
  }
}
