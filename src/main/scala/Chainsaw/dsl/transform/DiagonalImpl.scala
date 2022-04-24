package Chainsaw.dsl.transform

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import Chainsaw._
import Chainsaw.dsl._
import scala.reflect.ClassTag

class DiagonalImpl(coeff: Array[String], mult: Op2[Bits], multLatency: Int) extends HardImpl {
  val N = coeff.length
  override val spaceFold = factors(N)

  override val timeFold = Array(1)

  override def getImpl(spaceFold: Int, timeFold: Int) = {

    val impl: (Vec[Bits], Bool) => Vec[Bits] =
      if (spaceFold == 1) {
        (dataIn: Vec[Bits], last: Bool) => {
          Vec(coeff.map(B(_)).zip(dataIn).map { case (coeff, data) => mult(coeff, data) })
        }
      }
      else {
        (dataIn: Vec[Bits], last: Bool) => {
          val counter = CounterFreeRun(spaceFold)
          when(last)(counter.clear())
          val hardCoeff: Array[Vec[Bits]] = coeff.map(B(_)).grouped(N / spaceFold).toArray.map(Vec(_))
          val coeffROM: Mem[Vec[Bits]] = Mem(hardCoeff)
          val currentCoeff = coeffROM.readAsync(counter.value)
          Vec(currentCoeff.zip(dataIn).map { case (coeff, data) => mult(coeff, data) })
        }
      }

    RawImpl((N, N), impl, multLatency)

  }
}
