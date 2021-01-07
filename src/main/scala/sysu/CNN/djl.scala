package sysu.CNN

import ai.djl.ndarray.types.{DataType, Shape}
import ai.djl.ndarray._
import ai.djl.ndarray.index.NDIndex
import ai.djl._
import sysu.util.printArray2D

import ai.djl.nn.convolutional._

// todo 利用djl,将从模型开始的完整链条统一起来
// 真正的全栈工具,能够打通从算法到硬件的一切

/**
 * Scratch example to load model from path and do customized operations for the input and output.
 */
object DataProcessExample {

  // todo 通过隐式转换,使其成为Matrix的方法
  def Tensor2Matrix(tensor: NDArray) = { // design 此方法只能实现固定维度(二维)的转换
    val shape = tensor.getShape
    val shapes = (0 until shape.dimension()).map(i => shape.get(i))
    tensor.toArray.map(_.intValue()).grouped(shapes(1).toInt).toArray
  }

  // 是否还需要使用二维Array?还是干脆使用ndarray?
  // 不管数据生成方法上以谁为主,之后还是需要转换方法来打通java->scala->Spinal

  def main(args: Array[String]) {
    val manager = NDManager.newBaseManager()
    // creating
    val input2D = manager.arange(0,6).reshape(2,3)
    println(input2D.cumSum())
    println(input2D.get(new NDIndex(1,2)))
    val subs = input2D.split(2)
    println(input2D.transpose())
  }
}