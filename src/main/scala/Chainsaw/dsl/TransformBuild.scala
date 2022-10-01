package Chainsaw.dsl

import spinal.core._

object TransformBuild {

  def apply(impl: Impl, repetition: Repetition, reuse: Reuse) = {

    println(impl.getClass)

    val (inputSize, outputSize) = repetition.expand(impl.size)

    (dataIn: Vec[Bits]) => {
      require(dataIn.length == inputSize, s"input size should be $inputSize, while it is actually ${dataIn.length}")
      var segments = Seq(dataIn)

      // get segments according to space reuse
      repetition.space.reverse.foreach { rep =>
        segments = segments.map(segment => rep.divide(segment.toArray).map(Vec(_))).flatten
      }

      // iterate according to time reuse
//      val ret = segments.map { data =>
//        val current = impl.getImpl(1).impl // TODO: FINISH THIS
//        val pair = (data, False)
//        if (repetition.time.group == 1) current(pair)
//        else Array.iterate(pair, repetition.time.group + 1)(current).last
//      }.toArray
//
      val ret = segments.map { data =>
        println(s"init ${repetition.timeFactor} components of type ${impl.getClass}")
        val components = Array.fill(repetition.timeFactor)(impl.getImpl(1))
        components.head.dataIn.fragment := data
        components.head.dataIn.last := False
        components.prevAndNext { case (prev, next) => next.dataIn := prev.dataOut}
        (components.last.dataOut.fragment, components.last.dataOut.last)
      }.toArray

      val payload = Vec(ret.map(_._1).flatten)
      val last = ret.head._2

      require(payload.length == outputSize, s"output size should be $outputSize, while it is actually ${payload.length}")
      payload
    }
  }

}
