package Chainsaw.offer

import Chainsaw._
import spinal.core._
import spinal.core.sim.{SimConfig, _}

case class BasicComp() extends Component {
  val dataIn = in Vec(UInt(32 bits), 2)
//  val dataOut = out UInt (32 bits)
//  dataOut := Mux(dataIn(0) > dataIn(1), dataIn(0), dataIn(1))
  val dataOut = out Bool()
  dataOut := (dataIn(0) - dataIn(1)).msb
}

case class FindMax(width: Int, count: Int, pipelineGap:Int) extends Component {
  require(isPow2(count))
  val dataType = HardType(UInt(width bits))
  val dataIn = in Vec(dataType, count)
  val dataOut = out(dataType())

  def recursiveComparation(vec: Vec[UInt]): Vec[UInt] = {
    val N = vec.size
    val ret = N match {
      case 2 => Vec(Mux(vec(0) > vec(1), vec(0), vec(1)))
      case _ => {
        val left = recursiveComparation(Vec(vec.take(N / 2)))
        val right = recursiveComparation(Vec(vec.takeRight(N / 2)))
        Vec(left.zip(right).map { case (l, r) => Mux(l > r, l, r) })
      }
    }
    if(log2Up(N) % pipelineGap == 1) RegNext(ret) else ret
  }

  dataOut := recursiveComparation(dataIn).head
}

object FindMax extends App {
  //  SimConfig.withWave.compile(new FindMax(32, 32)).doSim { dut =>
  //
  //    dut.clockDomain.forkStimulus(2)
  //    dut.clockDomain.waitSampling(2)
  //
  //    def testOnce() = {
  //      dut.dataIn.foreach(_ #= DSPRand.nextInt(1 << 15))
  //      sleep(2)
  //    }
  //    testOnce()
  //    testOnce()
  //    testOnce()
  //    testOnce()
  //
  //    dut.clockDomain.waitSampling(10)
  //  }

    VivadoSynth(new FindMax(32, 32, 2))
}
