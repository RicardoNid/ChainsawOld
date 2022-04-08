package Chainsaw.dsl

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object Examples {

  def main(args: Array[String]): Unit = {

    implicit val field = IntField()

    implicit val vectorSpace = BasicVectorSpace[Int]

    //    val a = Matrix(Array.tabulate(3, 3)((_, _) => 1), 1, 1)
    //    val b = Matrix(Array.tabulate(3, 3)((_, _) => 2), 1, 1)

    //    println(a)
    //    println(b)
    //    println(a * b)

    SimConfig.withFstWave.compile(new Component {

      implicit val field = IntSignalField() //
      implicit val vectorSpace = BasicVectorSpace[SInt]

      val dataIn = in Vec(SInt(8 bits), 3)
      val dataOut = out Vec(SInt(13 bits), 3)

      val coeff = Matrix(Array.tabulate(3, 3)((i, j) => i + j), 1, 1)
      println(coeff)

      dataOut := (coeff *: dataIn.toMatrix).toVec.resized
    }).doSim { dut =>
      dut.dataIn.foreach(port => port #= 1)
      sleep(3)
    }

  }

}
