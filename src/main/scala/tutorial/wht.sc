// 如何定义数据重用距离?

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._
import spinal.lib.bus.amba4.axi._
import spinal.core.sim._

import sysu.xilinx._
import sysu.util._

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


//var i = 1
//var j = 0
//var state = 0
//var countDown = 17
//var state1Count = 0
//
//for (cycle <- 0 until 509) {
//  // state 1
//  if (state == 0) {
//    if (i < 15) {
//      if (i == j) {
//        i += 1
//        j = 0
//      }
//      else j += 1
//    }
//    else {
//      if (i == j) {
//        j = 0
//        state = 1
//      }
//      else j += 1
//    }
//    println(reuse(cycle + 3)(0), if (j < i - 1) 2 * i else 0, i, j, state)
//  }
//
//  if (state == 1) {
//    if (j == 15) {
//      j = 0
//      countDown -= 1
//      if (countDown == 0) state = 2
//    }
//    else j += 1
//    println(reuse(cycle + 3)(0), if (j < 15) 31 else 0, i, j, state)
//  }
//
//  if (state == 2) {
//    if (j == i - 2) {
//      i -= 1
//      j = 0
//      if (i == 2) state = 3
//    }
//    else j += 1
//    println(reuse(cycle + 3)(0), 2 * i, i, j, state)
//  }
//
//  if (state == 1) state1Count += 1
//}
//
//println(state1Count)