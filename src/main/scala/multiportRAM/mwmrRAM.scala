package multiportRAM

import spinal.core._

class mwmrRAM[T <: Data](dataType: HardType[T], wNum: Int, rNum: Int, depth: Int) extends Component {
  require(wNum > 0 && rNum > 0 && depth > 0)

  val io = new Bundle {
    val writeAddress = Vec(in UInt (log2Up(depth) bits), wNum)
    val writeData = Vec(in(dataType()), wNum)
    val writeEnable = Vec(in Bool(), wNum)

    val readAddress = Vec(in(UInt(log2Up(depth) bits)), rNum)
    val readData = Vec(out(dataType()), rNum)
    val readEnable = Vec(in Bool(), rNum)
  }

  val singleWPortArea = wNum == 1 generate new Area {
    //只有一个写入端口，此时不需要LVT
    val ram = new swmrRAM(dataType, rNum, depth)
    ram.io.writeAddress := io.writeAddress.head
    ram.io.writeData := io.writeData.head
    ram.io.writeEnable := io.writeEnable.head
    ram.io.readAddress := io.readAddress
    io.readData := ram.io.readData
    ram.io.readEnable := io.readEnable
  }

  val multiWPortArea = wNum > 1 generate new Area {
    //多个写入端口，实例化多个单口写入多口读出mem
    //使用LVT保存某个地址最后一次写入的mem
    //在读出时，用LVT中的值来选择对应的mem的读出的值
    val rams = Array.fill(wNum)(new swmrRAM(dataType, rNum, depth))
    val lvt = new LVT(wNum, rNum, depth)

    for (i <- 0 until wNum) {
      rams(i).io.writeAddress := io.writeAddress(i)
      rams(i).io.writeData := io.writeData(i)
      rams(i).io.writeEnable := io.writeEnable(i)
      rams(i).io.readAddress := io.readAddress
      rams(i).io.readEnable := io.readEnable
    }
    lvt.io.writeAddress := io.writeAddress
    lvt.io.writeEnable := io.writeEnable
    lvt.io.readAddress := io.readAddress
    lvt.io.readEnable := io.readEnable
    for (i <- 0 until rNum) {
      io.readData(i) := Vec(rams.map(_.io.readData(i)))(lvt.io.readData(i))
    }
  }
}

object mwmrRAM extends App {
  SpinalVerilog(new mwmrRAM(UInt(64 bits), 4, 4, 32))

  //  import spinal.core.sim._
  //
  //  SimConfig.compile(new mwmrRAM(UInt(16 bits), 8, 8, 256)).doSimUntilVoid { dut =>
  //    dut.clockDomain.forkStimulus(10)
  //
  //    fork {
  //      for (i <- 0 until 1000) {
  //        val address = (0 until 256).toArray
  //        for (j <- 0 until 8) {
  //          val idx = scala.util.Random.nextInt(256)
  //          val temp = address(j)
  //          address(j) = address(idx)
  //          address(idx) = temp
  //        }
  //        val data = Array.fill(8)(scala.util.Random.nextInt(1 << 16))
  //        for (j <- 0 until 8) {
  //          dut.io.writeAddress(j) #= address(j)
  //          dut.io.writeData(j) #= data(j)
  //          dut.io.readAddress(j) #= address(j)
  //        }
  //        dut.io.writeEnable.foreach(_ #= true)
  //        dut.io.readEnable.foreach(_ #= false)
  //        dut.clockDomain.waitSampling()
  //        dut.io.writeEnable.foreach(_ #= false)
  //        dut.io.readEnable.foreach(_ #= true)
  //        dut.clockDomain.waitSampling()
  //        dut.io.readEnable.foreach(_ #= false)
  //        dut.clockDomain.waitSampling()
  //        for (j <- 0 until 8) {
  //          if (dut.io.readData(j).toInt != data(j)) {
  //            print("出现错误。\n")
  //            simFailure()
  //          }
  //        }
  //      }
  //      simSuccess()
  //    }
  //  }
}