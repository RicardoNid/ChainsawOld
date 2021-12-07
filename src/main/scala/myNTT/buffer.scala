package myNTT

import spinal.core._

//缓存

class buffer[T <: Data](dataType: HardType[T], bufferDepth: Int) extends Component {

  val dataIn = in(dataType())
  val dataOut = out(dataType())

  val byPass = bufferDepth <= 0 generate new Area {
    dataOut := dataIn
  }

  val buffer = bufferDepth > 0 generate new Area {
    val regs = Vec(Reg(dataType()), bufferDepth)
    regs.head := dataIn
    dataOut := regs.last
    for (i <- 1 until bufferDepth) {
      regs(i) := regs(i - 1)
    }
  }
}