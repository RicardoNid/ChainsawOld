package Chainsaw.dsl.transform

import Chainsaw.dsl._
import spinal.core._
import spinal.lib._

class DiagonalImpl(coeff: Array[String], mult: Op2[Bits], multLatency: Int) extends Impl {
  val N = coeff.length
  override val foldMax = N
  override val size = (N, N)
  override def getImpl(fold:Int) = {

    val impl: ((Vec[Bits], Bool)) => (Vec[Bits], Bool) =
      if (fold == 1) {
        (dataIn: (Vec[Bits],Bool)) => {
          (Vec(coeff.map(B(_)).zip(dataIn._1).map { case (coeff, data) => mult(coeff, data) }), Delay(dataIn._2, multLatency, init = False))
        }
      }
      else {
        (dataIn: (Vec[Bits],Bool)) => {
          val counter = CounterFreeRun(fold)
          when(dataIn._2)(counter.clear())
          val hardCoeff: Array[Vec[Bits]] = coeff.map(B(_)).grouped(N / fold).toArray.map(Vec(_))
          val coeffROM: Mem[Vec[Bits]] = Mem(hardCoeff)
          val currentCoeff = coeffROM.readAsync(counter.value)
          (Vec(currentCoeff.zip(dataIn._1).map { case (coeff, data) => mult(coeff, data) }) , Delay(dataIn._2, multLatency, init = False))
        }
      }

    RawImpl(impl, multLatency)

  }
}
