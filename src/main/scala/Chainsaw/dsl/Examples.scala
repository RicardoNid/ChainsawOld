package Chainsaw.dsl

import Chainsaw._
import Chainsaw.dsl.ring._
import Chainsaw.dsl.transform._
import breeze.math._

import scala.util.Random

object Examples {

  def main(args: Array[String]): Unit = {

    implicit val finiteField: FiniteRing = FiniteRing(2)
    implicit val intField: UIntRing = UIntRing(4)
    implicit val complexField: ComplexRing = ComplexRing(5, 10)

    val data = Array.fill[FiniteInt](6)(0) ++ (0 until 128).map(_ => FiniteInt(Random.nextInt(2)))

    val conv = Matrix[FiniteInt](Array(
      Array[FiniteInt](1, 0, 0, 1, 1, 1, 1),
      Array[FiniteInt](1, 1, 0, 1, 1, 0, 1)))

    val sp16_16 = SPerm[FiniteInt](16, 16)
    val convert = new Converter(4, 1, finiteField, intField)
    val qam16 = LUT[Complex](
      Complex(-0.9486, 0.9486), Complex(-0.9486, 0.3162), Complex(-0.9486, -0.9486), Complex(-0.9486, -0.3162),
      Complex(-0.3162, 0.9486), Complex(-0.3162, 0.3162), Complex(-0.3162, -0.9486), Complex(-0.3162, -0.3162),
      Complex(0.9486, 0.9486), Complex(0.9486, 0.3162), Complex(0.9486, -0.9486), Complex(0.9486, -0.3162),
      Complex(0.3162, 0.9486), Complex(0.3162, 0.3162), Complex(0.3162, -0.9486), Complex(0.3162, -0.3162)
    )

    val ifft = Diagonal(Array.fill(64)(Complex(1, 1)))
    val ofdm = ifft ° (qam16 ⊗ 64) ° (convert ⊗ 64) ° sp16_16 ° (conv ⊗ (128, 1))

//    ofdm.randomTest(data, targetThroughput = 1 / 1.0)
//    ofdm.randomTest(data, targetThroughput = 1 / 2.0)
//    ofdm.randomTest(data, targetThroughput = 1 / 4.0)
//    ofdm.randomTest(data, targetThroughput = 1 / 8.0)
//
    val report1 =  VivadoSynth(ofdm.build(targetThroughput = 1 / 1.0), "system1_1")
    val report2 = VivadoSynth(ofdm.build(targetThroughput = 1 / 2.0), "system1_2")
    val report4 = VivadoSynth(ofdm.build(targetThroughput = 1 / 4.0), "system1_4")
    val report8 = VivadoSynth(ofdm.build(targetThroughput = 1 / 8.0), "system1_8")

    Seq(report1,report2,report4,report8).foreach(println)

    val sp4_4 = SPerm[FiniteInt](4, 4)
    val ifft4 = HSIFFT(4)
    val ofdmSmall = ifft4 ° (qam16 ⊗ 4) ° (convert ⊗ 4) ° sp4_4 ° (conv ⊗ (8, 1))
    GenRTL(ofdmSmall.build(targetThroughput = 1.0))

    val dataSmall = Array.fill[FiniteInt](6)(0) ++ (0 until 8).map(_ => FiniteInt(Random.nextInt(2)))
    ofdmSmall.randomTest(dataSmall, targetThroughput = 1)
  }
}
