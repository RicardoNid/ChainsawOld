package Chainsaw.dsl.transform

import Chainsaw.dsl.field.Field
import spinal.core._
import spinal.core.sim._

import scala.reflect.ClassTag

class Transform[TIn, TOut](val transform: Array[TIn] => Array[TOut],
                           val impl: Vec[Bits] => Vec[Bits],
                           val inputWidth: Int, val inputSize: Int, val outputSize: Int,
                           val parallel: Int, val iterative: Int)
                          (implicit tagIn: ClassTag[TIn], tagOut: ClassTag[TOut], fieldIn:Field[TIn], fieldOut: Field[TOut]) {

  def forceTransform(dataIn: Array[TIn]) = transform(dataIn).asInstanceOf[Array[TIn]]

  def apply(dataIn: Array[TIn]) = {
    dataIn.grouped(dataIn.length / parallel)
      .map(data => Array.iterate(data, iterative + 1)(forceTransform).last)
      .toArray.flatten.asInstanceOf[Array[TOut]]
  }

  def buildImpl = new Component {
    val input = in Vec(Bits(inputWidth bits), inputSize)
    val output = out(impl(input))
  }


  def build = new Component {
    val dataIn = in Vec(Bits(inputWidth bits), inputSize * parallel)
    val components = Seq.tabulate(parallel, iterative)((_,_) => buildImpl)
    components.foreach(row => row.init.zip(row.tail).foreach{ case (prev, next) => next.input := prev.output})
    val heads = components.map(_.head)
    dataIn.grouped(inputSize).toSeq // segments
      .zip(heads).foreach{ case (data, component) => component.input := Vec(data)}
    val lasts = components.map(_.last)
    val ret = Vec(lasts.map(_.output).flatten)
    val dataOut = out (cloneOf(ret))
    dataOut := ret
  }

  def composite[TPrev](that: Transform[TPrev, TIn])(implicit tag: ClassTag[TPrev], field: Field[TPrev]) = {
    val current = this
    val newTransform = (dataIn: Array[TPrev]) => current.apply(that.apply(dataIn))
    val newImpl = (dataIn: Vec[Bits]) => current.impl(that.impl(dataIn))
    new Transform[TPrev, TOut](newTransform, newImpl,
      that.inputWidth, that.inputSize, this.outputSize,
      1, 1)
  }

  // composition operator
  def *[TPrev](that: Transform[TPrev, TIn])(implicit tag: ClassTag[TPrev], field: Field[TPrev]) = composite(that)

  def ⊗(parallel: Int) = new Transform(transform, impl,
    this.inputWidth, this.inputSize, this.outputSize,
    this.parallel * parallel, iterative)


  def ^(iterative: Int) = new Transform(transform, impl,
    this.inputWidth, this.inputSize, this.outputSize,
    this.parallel, this.iterative * iterative)


  def test(data:Array[TIn]) = {
    SimConfig.withWave.compile(build).doSim{ dut =>
      dut.dataIn.zip(data.map(fieldIn.toBigInt)).foreach{ case (port, data) => port #= data}
      sleep(1)
      val golden = apply(data)
      val yours = dut.dataOut.map(_.toBigInt).map(fieldOut.fromBigInt)
      println(golden.mkString(" "))
      println(yours.mkString(" "))
      assert(golden.sameElements(yours))
      println("test passed")
    }
  }
}