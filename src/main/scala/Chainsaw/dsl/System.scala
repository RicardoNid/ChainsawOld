package Chainsaw.dsl

import spinal.core._
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer


/**
 * @param algo        algo composed by algos of sub-transforms
 * @param impls       the last element of impls is the beginning part of the system
 * @param repetitions repetition objects corresponding to impls
 */
class System[TIn, TOut](algo: Algo[TIn, TOut], impls: Seq[HardImpl], repetitions: Seq[Repetition],
                        val typeIn: MixType[TIn], val typeOut: MixType[TOut]) {

  def apply(dataIn: Array[TIn]) = algo(dataIn)

  def composite[TPrev](that: Transform[TPrev, TIn]) = {
    val newAlgo = (dataIn: Array[TPrev]) => this.apply(that.apply(dataIn))
    val newImpls = impls :+ that.base.impl
    val newRepetitions = repetitions :+ that.repetition
    new System[TPrev, TOut](newAlgo, newImpls, newRepetitions, that.typeIn, this.typeOut)
  }

  def °[TPrev](that: Transform[TPrev, TIn]) = composite(that)

  def buildImplForTransform(impl: HardImpl, repetition: Repetition) = {

    val (inputSize, outputSize) = repetition.expand(impl.size)

    (dataIn: Vec[Bits]) => {

      require(dataIn.length == inputSize, s"input size should be $inputSize, while it is actually ${dataIn.length}")
      var segments = Seq(dataIn)

      // get segments according to space reuse
      repetition.space.reverse.foreach { rep =>
        segments = segments.map(segment => rep.divide(segment.toArray).map(Vec(_))).flatten
      }

      // iterate according to time reuse
      val ret = segments.map { data =>
        val current = impl.getImpl(1, 1).impl // TODO: FINISH THIS
        val pair = (data,False)
        if (repetition.time.group == 1) current(pair)
        else Array.iterate(pair, repetition.time.group + 1)(current).last
      }.toArray

      val payload = Vec(ret.map(_._1).flatten)
      val last = ret.head._2

      require(payload.length == outputSize)
      payload
    }
  }

  def build =

  // without reuse
    new Component {
      val inputSize = repetitions.last.expand(impls.last.size)._1
      val outputSize = repetitions.head.expand(impls.head.size)._2
      val dataIn = in Vec(Bits(typeIn.width bits), inputSize)
      val dataOut = out Vec(Bits(typeOut.width bits), outputSize)

      val temp = ArrayBuffer(dataIn)
      impls.zip(repetitions).reverse.foreach { case (impl, repetition) =>
        val trans = buildImplForTransform(impl, repetition)
        temp += trans(temp.last)
      }

      dataOut := temp.last
    }


  def testOnce(stimuli: Array[TIn]) = {
    SimConfig.withFstWave.compile(build).doSim { dut =>
      dut.dataIn.zip(stimuli).foreach { case (port, data) => port #= typeIn.toBigInt(data) }
      sleep(2)
      val ret = dut.dataOut.map(_.toBigInt).map(typeOut.fromBigInt)
      println(s"yours : ${ret.mkString(" ")}")
      println(s"golden: ${apply(stimuli).mkString(" ")}")
    }

  }

}

object System {
  def apply[TIn, TOut]
  (algo: Algo[TIn, TOut], impls: Seq[HardImpl], repetitions: Seq[Repetition],
   typeIn: MixType[TIn], typeOut: MixType[TOut]): System[TIn, TOut] =
    new System(algo, impls, repetitions, typeIn, typeOut)
}
