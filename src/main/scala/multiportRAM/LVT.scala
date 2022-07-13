package multiportRAM

import spinal.core._

class LVT(wNum: Int, rNum: Int, depth: Int) extends Component {
  require(wNum > 1 && rNum > 0 && depth > 0)

  val io = new Bundle {
    val writeAddress = Vec(in UInt (log2Up(depth) bits), wNum)
    val writeEnable = Vec(in Bool(), wNum)

    val readAddress = Vec(in UInt (log2Up(depth) bits), rNum)
    val readData = Vec(out UInt (log2Up(wNum) bits), rNum)
    val readEnable = Vec(in Bool(), rNum)
  }

  val regs = Vec(Reg(UInt(log2Up(wNum) bits)), depth)
  //存有写入的mem的地址

  val writeArea = new Area {
    val writeEnable = Vec(Vec(False, wNum), depth)
    val writeData = Vec(Vec(U(0, log2Up(wNum) bits), wNum), depth)

    for (i <- 0 until wNum) {
      for (j <- 0 until depth) {
        when(io.writeAddress(i) === j) {
          writeEnable(j)(i) := io.writeEnable(i)
          when(io.writeEnable(i)) {
            writeData(j)(i) := U(i, log2Up(wNum) bits)
          }
        }
      }
    }

    for (i <- 0 until depth) {
      when(writeEnable(i).reduce(_ | _)) {
        regs(i) := writeData(i).reduce(_ | _)
      }
    }

    //多端口写若直接用regs(i):=的话
    //vivado无法综合
    //这里采用的是先与后或的结构
    //只要不出现某两个写端口同时有效且地址相同的情况，就不会出现问题
  }

  val readArea = new Area {
    for (i <- 0 until rNum) {
      io.readData(i) := RegNextWhen(regs(io.readAddress(i)), io.readEnable(i))
      //读发生时，打一拍
    }
  }
}

object LVT extends App {
  SpinalVerilog(new LVT(2, 2, 16))
}