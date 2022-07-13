package multiportRAM

import spinal.core._

class swmrRAM[T <: Data](dataType: HardType[T], rNum: Int, depth: Int) extends Component {
  require(rNum > 0 && depth > 0)

  val io = new Bundle {
    val writeAddress = in UInt (log2Up(depth) bits)
    val writeData = in(dataType())
    val writeEnable = in Bool()

    val readAddress = Vec(in(UInt(log2Up(depth) bits)), rNum)
    val readData = Vec(out(dataType()), rNum)
    val readEnable = Vec(in Bool(), rNum)
  }

  val mem = Array.fill(rNum)(Mem(dataType, depth))
  //单端口写入多端口读出的mem
  //实例化多个mem，写入发生时所有mem同时被写入
  //读出发生时，每个mem单独读出即可

  mem.foreach(_.write(io.writeAddress, io.writeData, io.writeEnable))

  for (i <- 0 until rNum) {
    io.readData(i) := mem(i).readSync(io.readAddress(i), io.readEnable(i))
    //一个周期延迟
  }
}

object swmrRAM extends App {
  SpinalVerilog(new swmrRAM(UInt(8 bits), 2, 16))
}