package Chainsaw.dsl.transform

import scala.reflect.ClassTag

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

class Transform[TIn, TOut](val transform: Array[TIn] => Array[TOut],
                           val impl: Vec[Bits] => Vec[Bits],
                           val inputWidth: Int, val inputSize: Int, val outputSize: Int,
                           val parallel: Int, val iterative: Int)
                          (implicit val tagIn: ClassTag[TIn], tagOut: ClassTag[TOut]) {

  def forceTransform(dataIn: Array[TIn]) = transform(dataIn).asInstanceOf[Array[TIn]]

  def apply(dataIn: Array[TIn]) = {
    dataIn.grouped(dataIn.length / parallel)
      .map(data => Array.iterate(data, iterative + 1)(forceTransform).last)
      .toArray.flatten.asInstanceOf[Array[TOut]]
  }

  def composite[TPrev](that: Transform[TPrev, TIn])(implicit tag: ClassTag[TPrev]) = {
    val current = this
    val newTransform = (dataIn: Array[TPrev]) => current.apply(that.apply(dataIn))
    val newImpl = (dataIn: Vec[Bits]) => current.impl(that.impl(dataIn))
    new Transform[TPrev, TOut](newTransform, newImpl,
      that.inputWidth, that.inputSize, this.outputSize,
      1, 1)
  }

  // composition operator
  def *[TPrev](that: Transform[TPrev, TIn])(implicit tag: ClassTag[TPrev]) = composite(that)

  def ⊗(parallel: Int) = new Transform(transform, impl,
    this.inputWidth, this.inputSize * parallel, this.outputSize * parallel,
    this.parallel * parallel, iterative)

  def ^(iterative: Int) = new Transform(transform, impl,
    this.inputWidth, this.inputSize, this.outputSize,
    this.parallel, this.iterative * iterative)

  def build = new Component {
    val dataIn = in Vec(Bits(inputWidth bits), inputSize)
    val dataOut = out(impl(dataIn))
  }
}