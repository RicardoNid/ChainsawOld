package DSP

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

//整个1394b物理层设计中的器件之间的通用接口
//接口由三部分组成，分别是数据端口，与数据同向的请求端口和与数据反向的请求端口
//数据端口用于输入/输出数据，其中master为数据输出模式
//与数据同向的请求端口是用于数据发送方的请求信号
//与数据反向的请求端口是用于数据接收方的请求信号

//请求是一种自发的信号，不是对对方请求的回应，是一种基于自己需求的信号
//在两方都有请求时，表明两方都有需求，此时数据交换才会发生

case class Interface(w: Int /*位宽*/ , p1: InterfaceGroup = null, p2: Int = 0) extends Bundle with IMasterSlave {

  val data: Bits = if (p1 == null) Bits(w bits) else p1.data(p2) //数据端口
  val w_req: Bool = if (p1 == null) Bool else p1.w_req(p2) //与数据同向的请求端口
  val r_req: Bool = if (p1 == null) Bool else p1.r_req(p2) //与数据反向的请求端口

  override def asMaster(): Unit = { //从Master视角看，数据端口和请求端口是输入端口，回复端口是输出端口
    out(data, w_req)
    in(r_req)
  }

  def getData(input: Interface): Unit = {
    data := input.data
    w_req := input.w_req
    input.r_req := r_req
  }

}

case class InterfaceGroup(w: Int /*位宽*/ , n: Int /*接口数量*/) extends Bundle with IMasterSlave {

  val data: Vec[Bits] = Vec(Bits(w bits), n) //数据端口
  val w_req: Bits = Bits(n bits) //与数据同向的请求端口
  val r_req: Bits = Bits(n bits) //与数据反向的请求端口

  override def asMaster(): Unit = { //从Master视角看，数据端口和请求端口是输入端口，回复端口是输出端口
    out(data, w_req)
    in(r_req)
  }

  def apply(i: Int): Interface = {
    val output = Interface(w, this, i)
    output
  }

  def getData(input: InterfaceGroup): Unit = {
    data := input.data
    w_req := input.w_req
    input.r_req := r_req
  }

}