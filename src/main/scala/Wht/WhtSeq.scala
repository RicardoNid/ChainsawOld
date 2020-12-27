package Wht

import spinal.core._
import sysu.util._
import sysu.xilinx._

class WhtSeq extends Component {

  val ori = Array.tabulate(32, 16)(_ * 16 + _)
  val map1 = (i: Int) => if (i > 15 && (i % 16 != 0)) i - 17 else -1
  val map2 = (i: Int) => if (i > 15) i - 16 else -1
  val map3 = (i: Int) => if (i % 16 != 0) i - 1 else -1
  // 原始数据及其依赖项
  val source = ori.map(_.map(i => Array(i, map1(i), map2(i), map3(i)))).flatten
  // 原始数据的实际顺序
  val reorder = ori.flatten.sortBy(i => ((i / 16) + (i % 16)) * 16 + (i / 16))
  // 按顺序重排的映射
  val remap = reorder.zipWithIndex.map {
    case (ele, i) => ele -> i
  }.toMap


  val stream = source.map(_.map(i => if (i >= 0) remap(i) else -1)).sortBy(_ (0))
  stream.map(_.mkString(" ")).mkString("\n")
  val switch = stream.map(_.map(i => if (i >= 0) 1 else 0))
  switch.map(_.mkString(" ")).mkString("\n")
  // 原始数据机器依赖项的delay关系
  val reuse = stream.zip(switch).map {
    case (row1, row2) => {
      Array(
        (row1(0) - row1(1)) * row2(1),
        (row1(0) - row1(2)) * row2(2),
        (row1(0) - row1(3)) * row2(3))
    }
  }
  //  println(reuse.map(_.mkString(" ")).mkString("\n"))
  //reuse.length = 512
  //reuse.map(_.max).max 最大重用距离 = 31
  reuse.map(_.count(_ == 31)).sum

  val seq = reuse.map(_ (0)).toArray


  val io = new Bundle {
    val output = out UInt (5 bits)
  }

  val innerSeqGen = new LUTSeqGen(seq)
  io.output := innerSeqGen.io.output
}

object WhtSeq extends App {
  val report = VivadoFlow(
    design = new WhtSeq(),
    vivadoConfig = recommended.vivadoConfig,
    vivadoTask = VivadoTask(topModuleName = "WhtSeq", workspacePath = "output/WhtSeq"),
    force = true
  ).doit()
  report.printArea
  report.printFMax
}


