package Chainsaw.dsl

import Chainsaw.dsl
import Chainsaw.dsl.dataflow.PeriodicFlow
import spinal.core._
import spinal.core.sim._
import spinal.lib.Delay

import scala.collection.mutable.ArrayBuffer


/**
 * @param algo        algo composed by algos of sub-transforms
 * @param impls       the last element of impls is the beginning part of the system
 * @param repetitions repetition objects corresponding to impls
 */
class System[TIn, TOut](algo: Algo[TIn, TOut], impls: Seq[Impl], repetitions: Seq[Repetition],
                        val typeIn: MixType[TIn], val typeOut: MixType[TOut]) {

  def apply(dataIn: Array[TIn]) = algo(dataIn)

  def composite[TPrev](that: Transform[TPrev, TIn]) = {
    val newAlgo = (dataIn: Array[TPrev]) => this.apply(that.apply(dataIn))
    val newImpls = impls :+ that.base.impl
    val newRepetitions = repetitions :+ that.repetition
    new System[TPrev, TOut](newAlgo, newImpls, newRepetitions, that.typeIn, this.typeOut)
  }

  def °[TPrev](that: Transform[TPrev, TIn]) = composite(that)

  val transforms = impls.zip(repetitions).reverse
  def getReuse(targetThroughput: Double) = transforms.map { transform => Reuse.findReuse(targetThroughput, transform._2, transform._1) }

  def build(targetThroughput: Double) = {

    new Component {
      val inputSize = repetitions.last.expand(impls.last.size)._1
      val outputSize = repetitions.head.expand(impls.head.size)._2
      val dataIn = in Vec(Bits(typeIn.width bits), inputSize)
      val lastIn = in Bool()
      val lastOut = out Bool()
      val dataOut = out Vec(Bits(typeOut.width bits), outputSize)

      // find reuse for each transform
      val transformsWithReuse = transforms.zip(getReuse(targetThroughput)).map{ case (transform, reuse) => (transform._1, transform._2, reuse)}
      println(transformsWithReuse.map { case (impl, repetition, reuse) =>
        s"${impl.getClass.toString} $reuse"
      }.mkString("\n↓\n"))

      val operators = transformsWithReuse.map((TransformBuild.apply _).tupled(_))
      val flows = transformsWithReuse.map { case (impl, repetition, reuse) => PeriodicFlow(impl.size, repetition, reuse) }
      val flowConverters = flows.init.zip(flows.tail)

      val dataPath = ArrayBuffer(dataIn)
      operators.foreach(op => dataPath += op(dataPath.last))
      dataOut := dataPath.last
      lastOut := Delay(lastIn, 20, init = False)
    }
  }

  def randomTest(stimuli: Array[TIn], targetThroughput: Double) = {

    Chainsaw.logger.info("start quick by-the-way random test")

    SimConfig.withFstWave.compile(build(targetThroughput)).doSim { dut =>
      dut.clockDomain.forkStimulus(2)
      dut.dataIn.zip(stimuli).foreach { case (port, data) => port #= typeIn.toBigInt(data) }
      dut.clockDomain.waitSampling(5)
      val ret = dut.dataOut.map(_.toBigInt).map(typeOut.fromBigInt)
      println(s"yours : ${ret.mkString(" ")}")
      println(s"golden: ${apply(stimuli).mkString(" ")}")
    }

  }

}

object System {
  def apply[TIn, TOut]
  (algo: Algo[TIn, TOut], impls: Seq[Impl], repetitions: Seq[Repetition],
   typeIn: MixType[TIn], typeOut: MixType[TOut]): System[TIn, TOut] =
    new System(algo, impls, repetitions, typeIn, typeOut)
}
