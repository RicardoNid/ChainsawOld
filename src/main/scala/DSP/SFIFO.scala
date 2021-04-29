package DSP

import spinal.core._
import spinal.lib._

//同步FIFO

case class SFIFO(w: Int /*位宽*/ , depth: Int /*深度，只能是2^n*/) extends Component {

  val io = new Bundle {

    val data_out = master(Interface(w))
    val data_in = slave(Interface(w))

  }

  val my_mem = Mem(Bits(w bits), depth) //存储元件
  val in_counter = Counter(2 * depth) //输入指针
  val out_counter = Counter(2 * depth) //输出指针

  io.data_out.w_req := True
  when(in_counter === out_counter) { //空
    io.data_out.w_req := False
  }

  io.data_in.r_req := True
  when(in_counter(log2Up(depth) - 1 downto 0) === out_counter(log2Up(depth) - 1 downto 0) &
    in_counter.msb === !out_counter.msb & !io.data_out.r_req) { //满且没有读
    io.data_in.r_req := False
  }

  when(io.data_in.w_req & io.data_in.r_req) { //写条件成立
    my_mem(in_counter(log2Up(depth) - 1 downto 0)) := io.data_in.data
    in_counter.increment()
  }

  when(io.data_out.w_req & io.data_out.r_req) { //读条件成立
    out_counter.increment()
  }

  io.data_out.data := my_mem(out_counter(log2Up(depth) - 1 downto 0))

}

object AFIFO_Vhdl {
  def main(arg: Array[String]): Unit = {
    SpinalVhdl(SFIFO(8, 16))
  }
}