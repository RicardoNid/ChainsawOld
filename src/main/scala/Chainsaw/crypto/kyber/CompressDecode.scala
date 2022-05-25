package Chainsaw.crypto.kyber

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.kyber.Reduce.{barrettReduce, csubq}
import Chainsaw.crypto.kyber._
import Chainsaw.crypto.symmetric._
import Chainsaw.dspTest._
import breeze.numerics.pow
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object CompressDecode {

  //todo :when SInt overfolow ,c code works fine

  //IN:ploy *a,OUT:128x8
  //Encode4(Compress(4)
  //ploy_compress in pack_ciphertext
  def polyCompressEn4(A: Array[SInt]): Array[Bits] = {
    require(A.forall(_.getWidth == 16) & A.size == 256)
    val Cs: Array[SInt] = A.map(a => csubq(a))
    val Ds: Array[Bits] = Cs.map(c => (((c << 4) + (q / 2)) / q).asBits.takeLow(4))
    Ds.grouped(2).map(_.reverse.reduce(_ ## _)).toArray
  }

  //IN:ployvec *a,OUT:640x8
  //Encode10(Compress(10)
  //polyvec_compress in pack_ciphertext
  def polyvecCompressEn10(A: Array[SInt]): Array[Bits] = {
    require(A.forall(_.getWidth == 16) && A.size == 512)
    val Cs = A.map(a => csubq(a))
    val Ds: Array[Array[Bits]] = Cs.map(c => (((c << 10) + (q / 2)) / q).takeLow(10)).grouped(4).toArray
    Ds.map(_.reverse.reduce(_ ##_).subdivideIn(8 bits)).flatten

  }


  //In ploy ; Out:32x8
  //encode1(Compress(x,1))
  //Poly_tomsg
  def polyCompressEn1(A: Array[SInt]): Array[Bits] = {
    require((A.forall(_.getWidth == 16)) && A.size == 256)
    val Cs = A.map(a => csubq(a))
    val Ds = Cs.map(c => (((c << 1) + (q / 2)) / q).takeLow(1))
    Ds.reverse.reduce(_ ## _).subdivideIn(8 bits).toArray
  }

  //IN:ploy *a,OUT:384x8
  //encode(12)
  //Polyvec_toBytes
  //without compress but with mode q
  //Used After mode q ,so  the actual bitwidth is less than 12
  def polyvecEn12(A: Array[SInt]): Array[Bits] = {
    require((A.forall(_.getWidth == 16)) && A.size == 512)
    val Cs = A.map(a => csubq(a).asBits)
    val Ds = Cs.map(_.takeLow(12)).grouped(2).toArray
    Ds.map(_.reverse.reduce(_##_).subdivideIn(8 bits)).flatten
  }


  // In 384*8 Out Polyvec:2*256*16
  //Decode12
  //polyvec_frombytes in unpack_ciphertext
  def polyvecDe12(A: Array[Bits]): Array[SInt] = {
    require((A.forall(_.getWidth == 8)) && A.size == 768 )
     A.grouped(3).map(_.reverse.reduce(_ ## _).subdivideIn(12 bits)).toArray.flatten.map(B(_, 16 bits).asSInt)
  }

  //IN 640*8 Out: ployvec 2*256816
  //Decompress(Decode10,10)
  //poly_decompress in unpack_ciphertext
  def polyvecDecompressDe10(A: Array[Bits]): Array[SInt] = {
    require((A.forall(_.getWidth == 8)) && A.size == 640 )
    val Bs: Array[UInt] = A.grouped(5).map(_.reverse.reduce(_ ## _).subdivideIn( 10 bits)).toArray.flatten.map(_.asUInt)
      Bs.map(c => (c * q + 512) >> 10).map(B(_, 12 bits).asSInt)

  }

  //In: 128*8 Out: poly 256*16
  //Decompress(Decode4,4)
  //poly_decompress in unpack_ciphertext
  def polyDecompressDe4(A: Array[Bits]): Array[SInt] = {
    require((A.forall(_.getWidth == 8)) && A.size == 128 )
    val Bs: Array[UInt] = A.map(_.subdivideIn(4 bits)).flatten.map(_.asUInt)
    Bs.map(c => (c * q + 8) >> 4).map(B(_, 16 bits).asSInt)

  }

  //In:ploy  Out:32*8
  //decompress(encode1,1)
  //poly_frommsg
  def polyDecompressDe1(A: Array[Bits]): Array[SInt] = {
    require((A.forall(_.getWidth == 8)) && A.size == 32 )
    val Bs: Array[UInt] = A.map(_.subdivideIn(1 bits)).flatten.map(_.asUInt)
    Bs.map(c => (c * q + 1) / 2).map(B(_, 16 bits).asSInt)
  }

  case class polyComEn4Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(16 bits), 256)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 128)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyCompressEn4(dataIn.payload.toArray.map(_.asSInt))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyvecComEn10Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(16 bits), 512)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 640)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyvecCompressEn10(dataIn.payload.toArray.map(_.asSInt))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyComEn1Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(16 bits), 256)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 32)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyCompressEn1(dataIn.payload.toArray.map(_.asSInt))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyvecEn12Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(16 bits), 512)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 768)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyvecEn12(dataIn.payload.toArray.map(_.asSInt))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyvecDe12Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 768)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(16 bits), 512)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyvecDe12(dataIn.payload.toArray).map(_.asBits)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyvecDe10Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 640)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(12 bits), 512)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyvecDecompressDe10(dataIn.payload.toArray).map(_.asBits)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyDe4Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 128)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyDecompressDe4(dataIn.payload.toArray).map(_.asBits)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyDe1Component() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 32)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyDecompressDe1(dataIn.payload.toArray).map(_.asBits)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }


  case class test() extends Component{
    val dataIn1 = in UInt (23 bits)
    val dataIn2 = in UInt (23 bits)

    val dataOut = out UInt (24 bits)
    val a1 =RegNext(dataIn1 +^ dataIn2)
    dataOut:=RegNext(a1/3329)
  }

  case class CompressDu(width:Int) extends Component{
    val dataIn = slave Flow UInt(24 bits)
    val dataOut = master Flow Bits(2*width bits)


    val a0 = dataIn.payload.takeLow(12).asUInt
    val a1 = dataIn.payload.takeHigh(12).asUInt
    val res0 = UInt(width bits)
    val res1 = UInt(width bits)
    dataOut.valid:=Delay(dataIn.valid,2,init = False)
    val b0 = Reg(UInt(24 bits))
    val b1 = Reg(UInt(24 bits))
    b0:= ((a0<<width) + U(1664,11 bits)).resized
    b1:= ((a1<<width) + U(1664,11 bits)).resized
    res0:=(b0/3329).resize(width)
    res1:=(b1/3329).resize(width)
    dataOut.payload:= RegNext(res1.asBits##res0.asBits)
    //     dataOut.payload :=dataIn.payload.asBits.takeLow(2*width)
  }

  def bar(a:UInt):UInt={
    val prod1 = a(23 downto 12)
    val prod2 = a(23 downto 14)
    val prod3 = a(23 downto 18)
    val prod4 = a(23 downto 20)

    val aa = RegNext(a(14 downto 0))
    val quo = RegNext((prod1.intoSInt +^ prod2.intoSInt -^ prod3.intoSInt -^ prod4.intoSInt).resize(15))

    val diff = (aa.intoSInt -^ (quo +^ (quo(3 downto 0)<<11) +^ (quo(4 downto 0)<<10)+^(quo(6 downto 0)<<8))).resize(15)

    val qmux = Reg(SInt(15 bits))
    val qdiv = Reg(UInt(12 bits))

    val quonext= RegNext(quo)
    val dif =RegNext(diff)
    switch(dif(14 downto 12).asUInt){
      is(0) {
        qmux:=0
        qdiv:= quonext.asUInt.resize(12)
      }
      is(1) {
        qmux := -3329
        qdiv:= quonext.asUInt.resize(12)+1
      }
      is(5) {
        qmux := 3 * q
        qdiv:=quonext.asUInt.resize(12)-3
      }
      is(6) {
        qmux := 3 * q
        qdiv:=quonext.asUInt.resize(12)-3
      }
      is(7) {
        qmux := 2 * q
        qdiv:=quonext.asUInt.resize(12)-2
      }
      default {
        qmux := 0
        qdiv:=quonext.asUInt.resize(12)
      }
    }


    val res= RegNext(RegNext(dif) + qmux)
    val out = Reg(UInt(12 bits))

    val qdivnext = Delay(qdiv,1)

  when(res>q){
     out:=qdivnext+1
   }.otherwise{
     out:=qdivnext
   }
    out
  }

  case class BAR() extends Component{
    val dataIn = slave Flow UInt(24 bits)
    val dataOut = master Flow Bits(12 bits)

    val a=bar(dataIn.payload)
    dataOut.payload:=a.asBits
    dataOut.valid:= Delay(dataIn.valid,3,init = False)
  }

  case class Compress1(width:Int) extends Component{
    val dataIn = slave Flow UInt(24 bits)
    val dataOut = master Flow Bits(2*width bits)

    val a0 = dataIn.payload.takeLow(12).asUInt
    val a1 = dataIn.payload.takeHigh(12).asUInt

    val b0 = Reg(UInt(24 bits))
    val b1 = Reg(UInt(24 bits))

    b0:= ((a0<<width) + 1664).resized
    b1:= ((a1<<width) + 1664).resized

    val divb0 = bar(b0)
    val divb1 = bar(b1)

    dataOut.payload:=divb1.resize(width).asBits ## divb0.resize(width).asBits
    dataOut.valid:=Delay(dataIn.valid,6,init = False)

  }


  def bar1(a:UInt):UInt={
    val prod1 = a(23 downto 12)
    val prod2 = a(23 downto 14)
    val prod3 = a(23 downto 18)
    val prod4 = a(23 downto 20)

    val aa = RegNext(a(14 downto 0))
    val quo = RegNext((prod1.intoSInt +^ prod2.intoSInt -^ prod3.intoSInt -^ prod4.intoSInt).resize(15))

    val diff = (aa.intoSInt -^ (quo +^ (quo(3 downto 0)<<11) +^ (quo(4 downto 0)<<10)+^(quo(6 downto 0)<<8))).resize(15)

    val qmux = (SInt(15 bits))
    val qdiv = (UInt(12 bits))

    switch(diff(14 downto 12).asUInt){
      is(0) {
        qmux:=0
        qdiv:= quo.asUInt.resize(12)
      }
      is(1) {
        qmux := -3329
        qdiv:= quo.asUInt.resize(12)+1
      }
      is(5) {
        qmux := 3 * q
        qdiv:=quo.asUInt.resize(12)-3
      }
      is(6) {
        qmux := 3 * q
        qdiv:=quo.asUInt.resize(12)-3
      }
      is(7) {
        qmux := 2 * q
        qdiv:=quo.asUInt.resize(12)-2
      }
      default {
        qmux := 0
        qdiv:=quo.asUInt.resize(12)
      }
    }
    val dif =RegNext(diff)

    val res= RegNext(dif + qmux)
    val out = Reg(UInt(12 bits))

    when(res>q){
      out:=RegNext(qdiv+1)
    }.otherwise{
      out:=RegNext(qdiv)
    }
    out
  }

  case class BAR1() extends Component{
    val dataIn = slave Flow UInt(24 bits)
    val dataOut = master Flow Bits(12 bits)

    val a=bar1(dataIn.payload)
    dataOut.payload:=a.asBits
    dataOut.valid:= Delay(dataIn.valid,3,init = False)
  }

  case class Compress11(width:Int) extends Component{
    val dataIn = slave Flow UInt(24 bits)
    val dataOut = master Flow Bits(2*width bits)

    val a0 = dataIn.payload.takeLow(12).asUInt
    val a1 = dataIn.payload.takeHigh(12).asUInt

    val b0 = Reg(UInt(24 bits))
    val b1 = Reg(UInt(24 bits))

    b0:= ((a0<<width) + 1664).resized
    b1:= ((a1<<width) + 1664).resized

    val divb0 = bar1(b0)
    val divb1 = bar1(b1)

    dataOut.payload:=divb1.resize(width).asBits ## divb0.resize(width).asBits
    dataOut.valid:=Delay(dataIn.valid,4,init = False)


  }


  case class Compress(width:Int) extends Component{
    val dataIn = slave Flow UInt(24 bits)
    val dataOut = master Flow Bits(2*width bits)

    val a0 = dataIn.payload.takeLow(12).asUInt
    val a1 = dataIn.payload.takeHigh(12).asUInt

    val b0 = Reg(UInt(width bits))
    val b1 = Reg(UInt(width bits))

    b0:= (((a0<<width) + 1664)/3329).resized
    b1:= (((a1<<width) + 1664)/3329).resized


    dataOut.payload:=b1 ## b0.asBits
    dataOut.valid:=Delay(dataIn.valid,1,init = False)


  }





  case class DeCompressDu(width:Int) extends Component{
    val dataIn = slave Flow Bits(2*width bits)
    val dataOut = master Flow Bits(24 bits)

    val a0 = dataIn.payload.takeLow(width).asUInt
    val a1 = dataIn.payload.takeHigh(width).asUInt
    val res0 = UInt(12 bits)
    val res1 = UInt(12 bits)

    val c = pow(2,width-1)
    val b0 = (a0*q + c).takeHigh(12)
    val b1 = (a1*q + c).takeHigh(12)

    dataOut.valid := Delay(dataIn.valid,1,init = False)
    dataOut.payload:= RegNext( b1.asBits ## b0.asBits )

  }

  case class encode4() extends Component{
    val dataIn = slave Flow Bits(8 bits)
    val dataOut = master Flow Bits(32 bits)
    val clear = in Bool()

    val reg32 = Reg(Bits(32 bits))
    val count4 =Counter(0,3)

    when(dataIn.valid){
      count4.increment()
      reg32:= (dataIn.payload##reg32).takeHigh(32)
    }.elsewhen(clear){
      count4:=0
    }

    val begin = Reg(Bool())init (False)
    begin:=count4.willOverflow
    when(begin){
      dataOut.valid:=True
    }.otherwise{
      dataOut.valid:=False
    }

    dataOut.payload:= reg32


  }

  case class encode10() extends Component{
    val dataIn = slave Flow Bits(20 bits)
    val dataOut = master Flow Bits(32 bits)
    val clear = in Bool()
    val reg160 = Reg(Bits(48 bits))
    val count8 =Counter(0,7)

    when(dataIn.valid){
      count8.increment()
      reg160:= (dataIn.payload ## reg160).takeHigh(48)
    }.elsewhen(clear){
      count8:=0
    }



    val begin = RegNext(dataIn.valid,init = False)
    when(begin){
      when(count8===2){
        dataOut.valid:=True
        dataOut.payload:= reg160(39 downto 8)
      }.elsewhen(count8===4){
        dataOut.valid:=True
        dataOut.payload:= reg160(31 downto 0)
      }.elsewhen(count8===5){
        dataOut.valid:=True
        dataOut.payload:= reg160(43 downto 12)
      }.elsewhen(count8===7){
        dataOut.valid:=True
        dataOut.payload:= reg160(35 downto 4)
      }.elsewhen(count8===0){
        dataOut.valid:=True
        dataOut.payload:= reg160(47 downto 16)
      }.otherwise{
        dataOut.valid:=False
        dataOut.payload:=0
      }
    }.otherwise{
      dataOut.valid:=False
      dataOut.payload:=0
    }





  }

  case class encode12() extends Component{
    val dataIn = slave Flow Bits(24 bits)
    val dataOut = master Flow Bits(32 bits)
    val clear = in Bool()

    val reg48 = Reg(Bits(48 bits))
    val count4 = Counter(0,3)

    when(dataIn.valid){
      count4.increment()
      reg48:= (dataIn.payload##reg48).takeHigh(48)
    }.elsewhen(clear){
      count4:=0
    }

    val begin = RegNext(dataIn.valid,init = False)
    val outValid = Reg(Bool())init (False)
    when(count4===0){
      outValid:=False
    }.otherwise{
      outValid:=True
    }

    when(begin){
      when(count4===2){
        dataOut.payload:=reg48(31 downto 0)
      }.elsewhen(count4===3){
        dataOut.payload:=reg48(39 downto 8)
      }.elsewhen(count4===0){
        dataOut.payload:=reg48(47 downto 16)
      }.otherwise{
        dataOut.payload:=0
      }
    }.otherwise{
      dataOut.payload:=0
    }

    dataOut.valid:=outValid

  }

  case class fifoDecode(depth:Int) extends Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut20 = master Flow Bits(20 bits)
    val dataOut8 = master Flow Bits(8 bits)
    val dataOut24 = master Flow Bits(24 bits)
    val param = in UInt(2 bits)   //0- 20   1- 8  2- 24

//param的值为0时进Encode10，输出20,8个周期输出5个数据
//Param的值为1时，encode4，输出8，4个周期输出1个数据
//param为3时，输出ecode12，输出24，4个周期输出3个数据

    val fifo = StreamFifo(Bits(32 bits),depth)
    fifo.io.push.valid:=dataIn.valid
    fifo.io.push.payload:=dataIn.payload

    val count4 = Counter(0,3)
    val reg32 = Reg(Bits(32 bits)) init(0)
    val outValid8 = Reg(Bool()) init(False)

    val count8 = Counter(0,7)
    val reg48 = Reg(Bits(48 bits)) init(0)
    val outValid20 = Reg(Bool()) init(False)

    val count24 = Counter(0,3)
    val reg24 = Reg(Bits(48 bits))
    val outValid = Reg(Bool()) init(False)

    //

    when(param===0){  //
      outValid8 :=False
      outValid:=False
      count4:=0
      count24:=0
      when(fifo.io.pop.valid & fifo.io.pop.ready){
        count8.increment()
        outValid20:=True
        reg48 := (fifo.io.pop.payload ##reg48 ).takeHigh(48)
      }.elsewhen(count8===2 | count8 ===5 | count8 ===7){
        outValid20:=True
        count8.increment()
      }.otherwise{
        outValid20:=False
      }

      when(count8 === 2 || count8 ===5 || count8===7){
        fifo.io.pop.ready:=False
      }.otherwise{
        fifo.io.pop.ready:=True
      }
    }.elsewhen(param===1){
      outValid20:=False
      outValid:=False
      count24:=0
      count8:=0
      when(fifo.io.pop.valid || (count4 =/= 0)){
        count4.increment()
        outValid8:=True
        when(count4===0){
          reg32:=fifo.io.pop.payload
        }
      }.otherwise{
        outValid8 :=False
      }

      when(count4===0){
        fifo.io.pop.ready:= True
      }.otherwise{
        fifo.io.pop.ready:=False
      }
    }.otherwise{
      outValid20:=False
      outValid8:=False
      count8:=0
      count4:=0
      when(fifo.io.pop.valid && fifo.io.pop.ready){
        count24.increment()
        outValid:=True
        reg24:=(fifo.io.pop.payload ## reg24).takeHigh(48)
      }.elsewhen(count24===3){
        count24.increment()
        outValid:=True
      }.otherwise{
        outValid:=False
      }

      when(count24===3){
        fifo.io.pop.ready:=False
      }.otherwise{
        fifo.io.pop.ready:=True

      }
    }


    when(count8===0){
      dataOut20.payload:=reg48(47 downto 28)
    }.elsewhen(count8===1){
      dataOut20.payload:=reg48(35 downto 16)
    }.elsewhen(count8===2){
      dataOut20.payload:=reg48(23 downto 4)
    }.elsewhen(count8===3){
      dataOut20.payload:=reg48(43 downto 24)
    }.elsewhen(count8===4){
      dataOut20.payload:=reg48(31 downto 12)
    }.elsewhen(count8===5){
      dataOut20.payload:=reg48(19 downto 0)
    }.elsewhen(count8===6){
      dataOut20.payload:=reg48(39 downto 20)
    }.otherwise{
      dataOut20.payload:=reg48(27 downto 8)
    }

    dataOut20.valid:= outValid20

    when(count4 ===0 ){
      dataOut8.payload:=reg32(31 downto 24)
    }.elsewhen(count4===1){
      dataOut8.payload:=reg32(7 downto 0)
    }.elsewhen(count4===2){
      dataOut8.payload:=reg32(15 downto 8)
    }.otherwise{
      dataOut8.payload:=reg32(23 downto 16)
    }

    dataOut8.valid:= outValid8

    when(count24===0){
      dataOut24.payload:=reg24(47 downto 24)
    }.elsewhen(count24===1){
      dataOut24.payload:=reg24(39 downto 16)
    }.elsewhen(count24===2){
      dataOut24.payload:=reg24(31 downto 8)
    }.otherwise{
      dataOut24.payload:=reg24(23 downto 0)
    }
    dataOut24.valid:= outValid

  }

  case class fifoDecode12(width: Int) extends Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut = master Flow Bits(24 bits)

    val fifo = StreamFifo(Bits(32 bits),width)

    val count4 = Counter(0,3)
    val reg48 = Reg(Bits(48 bits))
    val outValid = Reg(Bool()) init(False)

    fifo.io.push.valid:=dataIn.valid
    fifo.io.push.payload:=dataIn.payload

    when(fifo.io.pop.valid && fifo.io.pop.ready){
      count4.increment()
      outValid:=True
        reg48:=(fifo.io.pop.payload ## reg48).takeHigh(48)
    }.elsewhen(count4===3){
      count4.increment()
      outValid:=True
    }.otherwise{
      outValid:=False
    }

    when(count4===0){
      dataOut.payload:=reg48(47 downto 24)
      fifo.io.pop.ready:=True
    }.elsewhen(count4===1){
      dataOut.payload:=reg48(39 downto 16)
      fifo.io.pop.ready:=True
    }.elsewhen(count4===2){
      dataOut.payload:=reg48(31 downto 8)
      fifo.io.pop.ready:=True
    }.otherwise{
      dataOut.payload:=reg48(23 downto 0)
      fifo.io.pop.ready:=False
    }
    dataOut.valid:= outValid

  }



  case class  deComDecode1() extends Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut = master Flow Bits(24 bits)
    val work = in Bool()


    val fifo =StreamFifo(Bits(32 bits),8)

    fifo.io.push.valid:=dataIn.valid
    fifo.io.push.payload:=dataIn.payload

    //decode1 2bits
    val reg32 = Reg(Bits(32 bits))
    val count16 = Counter(0,15)
    val DecodeValid = Reg(Bool()) init(False)
    val DecodeOut = Bits(2 bits)

    when(fifo.io.pop.fire ){
      reg32:=fifo.io.pop.payload
      count16.increment()
      DecodeValid:=True
    }.elsewhen(count16=/=0){
      DecodeValid:=True
      reg32:= reg32|>>2
      count16.increment()
    }.otherwise{
      DecodeValid:=False
    }

    when(work){
      when(count16===0){
        fifo.io.pop.ready:=True
      }.otherwise{
        fifo.io.pop.ready:=False
      }
    }.otherwise{
      fifo.io.pop.ready:=False
    }

    DecodeOut := reg32.takeLow(2)

    //deCompress

    val a0 = RegNext(DecodeOut(0))
    val a1 = RegNext(DecodeOut(1))

    when(a0===True){
      dataOut.payload(11 downto 0):= B(1665,12 bits) //(q+1)/2
    }.otherwise{
      dataOut.payload(11 downto 0):=B(0,12 bits)
    }
    when(a1===True){
      dataOut.payload(23 downto 12):= B(1665,12 bits) //(q+1)/2
    }.otherwise{
      dataOut.payload(23 downto 12):=B(0,12 bits)
    }

    dataOut.valid:=RegNext(DecodeValid,init = False)



  }


  case class  deComDe1() extends Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut = master Flow Bits(24 bits)
    val work = in Bool()
    val fifoReady = out Bool()


    //decode1 2bits
    val reg32 = Reg(Bits(32 bits))
    val count16 = Counter(0,15)
    val DecodeValid = Reg(Bool()) init(False)
    val DecodeOut = Bits(2 bits)

    when(dataIn.valid & fifoReady ){
      reg32:=dataIn.payload
      count16.increment()
      DecodeValid:=True
    }.elsewhen(count16=/=0){
      DecodeValid:=True
      reg32:= reg32|>>2
      count16.increment()
    }.otherwise{
      DecodeValid:=False
    }

    when(work){
      when(count16===0){
        fifoReady:=True
      }.otherwise{
        fifoReady:=False
      }
    }.otherwise{
      fifoReady:=False
      count16:=0
    }

    DecodeOut := reg32.takeLow(2)

    //deCompress

    val a0 = RegNext(DecodeOut(0))
    val a1 = RegNext(DecodeOut(1))

    when(a0===True){
      dataOut.payload(11 downto 0):= B(1665,12 bits) //(q+1)/2
    }.otherwise{
      dataOut.payload(11 downto 0):=B(0,12 bits)
    }
    when(a1===True){
      dataOut.payload(23 downto 12):= B(1665,12 bits) //(q+1)/2
    }.otherwise{
      dataOut.payload(23 downto 12):=B(0,12 bits)
    }

    dataOut.valid:=RegNext(DecodeValid,init = False)



  }

  case class ComEncode1() extends Component{
    val dataIn = slave Flow Bits(24 bits)
    val dataOut = master Flow Bits(32 bits)

    //Compress1
    val CompressOut = Bits(2 bits)
    val CompressValid = RegNext(dataIn.valid,init=False)

    val b0 = RegNext(((dataIn.payload(11 downto 0 )<<1).asUInt  + 1664)/3329)
    val b1 = RegNext(((dataIn.payload(23 downto 12)<<1).asUInt + 1664)/3329)

    when(b0 === 1){CompressOut(0):=True}.otherwise(CompressOut(0):=False)
    when(b1 === 1){CompressOut(1):=True}.otherwise(CompressOut(1):=False)

    val count16 = Counter(0,15)
    val reg32 = Reg(Bits(32 bits))
    when(CompressValid){
      count16.increment()
      reg32:=(CompressOut##reg32).takeHigh(32)
    }

    dataOut.payload:=reg32
    dataOut.valid:=Delay(count16.willOverflow,1,init = False)

  }

  case class Decode12() extends Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut = master Flow Bits(24 bits)
    val fifoReady = out Bool()

    val count4 = Counter(0,3)
    val reg48 = Reg(Bits(48 bits))
    val outValid = Reg(Bool()) init(False)


    when(dataIn.valid&fifoReady){
      count4.increment()
      outValid:=True
      reg48:=(dataIn.payload ## reg48).takeHigh(48)
    }.elsewhen(count4===3){
      count4.increment()
      outValid:=True
    }.otherwise{
      outValid:=False
    }

    when(count4===0){
      dataOut.payload:=reg48(47 downto 24)
      fifoReady:=True
    }.elsewhen(count4===1){
      dataOut.payload:=reg48(39 downto 16)
      fifoReady:=True
    }.elsewhen(count4===2){
      dataOut.payload:=reg48(31 downto 8)
      fifoReady:=True
    }.otherwise{
      dataOut.payload:=reg48(23 downto 0)
      fifoReady:=False
    }
    dataOut.valid:= outValid

  }

  case class Decode10() extends  Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut20 = master Flow Bits(20 bits)
    val fifoReady = out Bool()

    val count8 = Counter(0,7)
    val reg48 = Reg(Bits(48 bits)) init(0)
    val outValid20 = Reg(Bool()) init(False)

    when(dataIn.valid & fifoReady){
      count8.increment()
      outValid20:=True
      reg48 := (dataIn.payload ##reg48 ).takeHigh(48)
    }.elsewhen(count8===2 | count8 ===5 | count8 ===7){
      outValid20:=True
      count8.increment()
    }.otherwise{
      outValid20:=False
    }

    when(count8 === 2 || count8 ===5 || count8===7){
      fifoReady:=False
    }.otherwise{
      fifoReady:=True
    }

    when(count8===0){
      dataOut20.payload:=reg48(47 downto 28)
    }.elsewhen(count8===1){
      dataOut20.payload:=reg48(35 downto 16)
    }.elsewhen(count8===2){
      dataOut20.payload:=reg48(23 downto 4)
    }.elsewhen(count8===3){
      dataOut20.payload:=reg48(43 downto 24)
    }.elsewhen(count8===4){
      dataOut20.payload:=reg48(31 downto 12)
    }.elsewhen(count8===5){
      dataOut20.payload:=reg48(19 downto 0)
    }.elsewhen(count8===6){
      dataOut20.payload:=reg48(39 downto 20)
    }.otherwise{
      dataOut20.payload:=reg48(27 downto 8)
    }

    dataOut20.valid:= outValid20


  }

  case class Decode4() extends  Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut8 = master Flow Bits(8 bits)
    val fifoReady = out Bool()

    val count4 = Counter(0,3)
    val reg32 = Reg(Bits(32 bits)) init(0)
    val outValid8 = Reg(Bool()) init(False)


    when(dataIn.valid &fifoReady){
      count4.increment()
      outValid8:=True
      reg32 := dataIn.payload
    }.elsewhen(count4=/=0){
      count4.increment()
      outValid8:=True
    }otherwise{
      outValid8 :=False
    }

    when(count4===0){
      fifoReady:= True
    }.otherwise{
      fifoReady:=False
    }

    when(count4 ===0 ){
      dataOut8.payload:=reg32(31 downto 24)
    }.elsewhen(count4===1){
      dataOut8.payload:=reg32(7 downto 0)
    }.elsewhen(count4===2){
      dataOut8.payload:=reg32(15 downto 8)
    }.otherwise{
      dataOut8.payload:=reg32(23 downto 16)
    }
    dataOut8.valid:= outValid8
  }

  case class testDecode12() extends Component{
    val dataIn = slave Flow Bits(32 bits)
    val dataOut = master Flow Bits(8 bits)

    val fifo = StreamFifo(Bits(32 bits),10)
    val de12 = Decode4()

    fifo.io.push.valid:=dataIn.valid
    fifo.io.push.payload:=dataIn.payload
    fifo.io.pop.ready:=de12.fifoReady

    de12.dataIn.payload:= fifo.io.pop.payload
    de12.dataIn.valid:= fifo.io.pop.fire
    dataOut:=de12.dataOut8



  }



}

object TestCompressDecode {
  def main(args: Array[String]): Unit = {
    val testvalue = Seq.fill(256)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk * 128)
    }
    val testvalue2 = Seq.fill(512)(0)
      .zipWithIndex.map {
      case (i, kk) => BigInt(kk * 64 + 256)
    }

    val testvalue1 = Seq.fill(512)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk * 8)
    }

    val testvalueBits = Seq.fill(128)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk)
    }

    val testvalueBits12 = Seq.fill(768)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk / 3)
    }

    val testvalueBits1 = Seq.fill(32)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk)
    }
    val testvalueBits10 = Seq.fill(640)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk / 3)
    }


//    VivadoSynth(CompressDecode.BAR())
//    VivadoSynth(CompressDecode.BAR1())

    VivadoSynth(CompressDecode.Compress11(10))
    VivadoSynth(CompressDecode.Compress1(10))

    val Goldenen4 = Seq("16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "85", "118", "135", "153", "170", "203", "220", "237", "255", "16", "33", "50", "68", "85", "118", "135", "153", "170", "203", "220").map(BigInt(_, 10))
    val Goldenen1 = Seq("128", "255", "15", "0", "254", "63", "0", "248", "255", "0", "224", "255", "3", "128", "255", "15", "0", "254", "63", "0", "248", "255", "0", "224", "255", "3", "128", "255", "15", "0", "254", "63").map(BigInt(_, 10))
    val Goldenen10 = Seq("79", "136", "97", "135", "34", "157", "196", "82", "76", "54", "236", "0", "68", "209", "73", "59", "61", "37", "150", "93", "138", "117", "22", "91", "113", "216", "177", "7", "32", "133", "39", "238", "248", "164", "152", "118", "42", "218", "105", "172", "197", "98", "203", "46", "192", "19", "159", "188", "243", "211", "98", "219", "173", "120", "231", "177", "23", "143", "61", "251", "0", "76", "112", "194", "14", "78", "136", "97", "71", "34", "157", "196", "82", "12", "54", "236", "0", "52", "209", "73", "59", "57", "37", "150", "93", "137", "117", "22", "27", "113", "216", "177", "7", "224", "132", "39", "238", "232", "164", "152", "118", "38", "218", "105", "172", "196", "98", "203", "238", "191", "19", "159", "188", "179", "211", "98", "219", "157", "120", "231", "177", "19", "143", "61", "251", "255", "79", "112", "130", "14", "78", "136", "97", "71", "34", "157", "196", "66", "12", "54", "236", "252", "51", "209", "73", "58", "57", "37", "86", "93", "137", "117", "6", "27", "113", "216", "177", "247", "223", "132", "39", "234", "232", "164", "152", "117", "38", "218", "41", "172", "196", "98", "187", "238", "191", "19", "159", "172", "179", "211", "98", "215", "157", "120", "231", "176", "19", "143", "253", "250", "255", "79", "96", "130", "14", "78", "136", "81", "71", "34", "157", "192", "66", "12", "54", "235", "252", "51", "145", "73", "58", "57", "21", "86", "93", "137", "113", "6", "27", "113", "216", "173", "247", "223", "132", "38", "234", "232", "100", "152", "117", "38", "202", "41", "172", "196", "94", "187", "238", "191", "19", "155", "172", "179", "211", "97", "215", "157", "56", "231", "176", "19", "127", "253", "250", "255", "75", "96", "130", "14", "78", "132", "81", "71", "34", "156", "192", "66", "204", "53", "235", "252", "35", "145", "73", "58", "53", "21", "86", "93", "136", "113", "6", "27", "113", "215", "173", "247", "159", "132", "38", "234", "216", "100", "152", "117", "34", "202", "41", "172", "195", "94", "187", "238", "191", "18", "155", "172", "115", "211", "97", "215", "141", "56", "231", "176", "15", "127", "253", "250", "254", "75", "96", "130", "14", "77", "132", "81", "7", "34", "156", "192", "50", "204", "53", "235", "248", "35", "145", "73", "57", "53", "21", "86", "93", "136", "113", "6", "219", "112", "215", "173", "231", "159", "132", "38", "230", "216", "100", "152", "116", "34", "202", "233", "171", "195", "94", "187", "174", "191", "18", "155", "156", "115", "211", "97", "211", "141", "56", "231", "175", "15", "127", "189", "250", "254", "75", "96", "66", "14", "77", "132", "65", "7", "34", "156", "188", "50", "204", "53", "234", "248", "35", "81", "73", "57", "53", "21", "22", "93", "136", "113", "246", "218", "112", "215", "169", "231", "159", "132", "37", "230", "216", "36", "152", "116", "34", "186", "233", "171", "195", "94", "171", "174", "191", "18", "151", "156", "115", "211", "96", "211", "141", "248", "230", "175", "15", "111", "189", "250", "254", "75", "80", "66", "14", "77", "128", "65", "7", "34", "155", "188", "50", "140", "53", "234", "248", "19", "81", "73", "57", "53", "5", "22", "93", "136", "109", "246", "218", "112", "214", "169", "231", "95", "132", "37", "230", "200", "36", "152", "116", "30", "186", "233", "171", "195", "90", "171", "174", "191", "17", "151", "156", "51", "211", "96", "211", "125", "248", "230", "175", "11", "111", "189", "250", "254", "71", "80", "66", "14", "76", "128", "65", "199", "33", "155", "188", "34", "140", "53", "234", "244", "19", "81", "73", "57", "49", "5", "22", "93", "135", "109", "246", "154", "112", "214", "169", "215", "95", "132", "37", "226", "200", "36", "152", "116", "30", "186", "233", "171", "194", "90", "171", "110", "191", "17", "151", "140", "51", "211", "96", "207", "125", "248", "230", "174", "11", "111", "189", "250", "253", "71", "80", "2", "14", "76", "128", "49", "199", "33", "155", "184", "34", "140", "53", "233", "244", "19", "81", "73", "56", "49", "5", "214", "92", "135", "109", "230", "154", "112", "214", "165", "215", "95", "132", "36", "226", "200", "36", "152", "115", "30", "186", "169", "171", "194", "90", "155", "110", "191", "17", "147", "140", "51", "211", "95", "207", "125", "184", "230").map(BigInt(_, 10))
    val Goldenen12 = Seq("0", "128", "0", "16", "128", "1", "32", "128", "2", "48", "128", "3", "64", "128", "4", "80", "128", "5", "96", "128", "6", "112", "128", "7", "128", "128", "8", "144", "128", "9", "160", "128", "10", "176", "128", "11", "192", "128", "12", "208", "128", "13", "224", "128", "14", "240", "128", "15", "0", "129", "16", "16", "129", "17", "32", "129", "18", "48", "129", "19", "64", "129", "20", "80", "129", "21", "96", "129", "22", "112", "129", "23", "128", "129", "24", "144", "129", "25", "160", "129", "26", "176", "129", "27", "192", "129", "28", "208", "129", "29", "224", "129", "30", "240", "129", "31", "0", "130", "32", "16", "130", "33", "32", "130", "34", "48", "130", "35", "64", "130", "36", "80", "130", "37", "96", "130", "38", "112", "130", "39", "128", "130", "40", "144", "130", "41", "160", "130", "42", "176", "130", "43", "192", "130", "44", "208", "130", "45", "224", "130", "46", "240", "130", "47", "0", "131", "48", "16", "131", "49", "32", "131", "50", "48", "131", "51", "64", "131", "52", "80", "131", "53", "96", "131", "54", "112", "131", "55", "128", "131", "56", "144", "131", "57", "160", "131", "58", "176", "131", "59", "192", "131", "60", "208", "131", "61", "224", "131", "62", "240", "131", "63", "0", "132", "64", "16", "132", "65", "32", "132", "66", "48", "132", "67", "64", "132", "68", "80", "132", "69", "96", "132", "70", "112", "132", "71", "128", "132", "72", "144", "132", "73", "160", "132", "74", "176", "132", "75", "192", "132", "76", "208", "132", "77", "224", "132", "78", "240", "132", "79", "0", "133", "80", "16", "133", "81", "32", "133", "82", "48", "133", "83", "64", "133", "84", "80", "133", "85", "96", "133", "86", "112", "133", "87", "128", "133", "88", "144", "133", "89", "160", "133", "90", "176", "133", "91", "192", "133", "92", "208", "133", "93", "224", "133", "94", "240", "133", "95", "0", "134", "96", "16", "134", "97", "32", "134", "98", "48", "134", "99", "64", "134", "100", "80", "134", "101", "96", "134", "102", "112", "134", "103", "128", "134", "104", "144", "134", "105", "160", "134", "106", "176", "134", "107", "192", "134", "108", "208", "134", "109", "224", "134", "110", "240", "134", "111", "0", "135", "112", "16", "135", "113", "32", "135", "114", "48", "135", "115", "64", "135", "116", "80", "135", "117", "96", "135", "118", "112", "135", "119", "128", "135", "120", "144", "135", "121", "160", "135", "122", "176", "135", "123", "192", "135", "124", "208", "135", "125", "224", "135", "126", "240", "135", "127", "0", "136", "128", "16", "136", "129", "32", "136", "130", "48", "136", "131", "64", "136", "132", "80", "136", "133", "96", "136", "134", "112", "136", "135", "128", "136", "136", "144", "136", "137", "160", "136", "138", "176", "136", "139", "192", "136", "140", "208", "136", "141", "224", "136", "142", "240", "136", "143", "0", "137", "144", "16", "137", "145", "32", "137", "146", "48", "137", "147", "64", "137", "148", "80", "137", "149", "96", "137", "150", "112", "137", "151", "128", "137", "152", "144", "137", "153", "160", "137", "154", "176", "137", "155", "192", "137", "156", "208", "137", "157", "224", "137", "158", "240", "137", "159", "0", "138", "160", "16", "138", "161", "32", "138", "162", "48", "138", "163", "64", "138", "164", "80", "138", "165", "96", "138", "166", "112", "138", "167", "128", "138", "168", "144", "138", "169", "160", "138", "170", "176", "138", "171", "192", "138", "172", "208", "138", "173", "224", "138", "174", "240", "138", "175", "0", "139", "176", "16", "139", "177", "32", "139", "178", "48", "139", "179", "64", "139", "180", "80", "139", "181", "96", "139", "182", "112", "139", "183", "128", "139", "184", "144", "139", "185", "160", "139", "186", "176", "139", "187", "192", "139", "188", "208", "139", "189", "224", "139", "190", "240", "139", "191", "0", "140", "192", "16", "140", "193", "32", "140", "194", "48", "140", "195", "64", "140", "196", "80", "140", "197", "96", "140", "198", "112", "140", "199", "128", "140", "200", "144", "140", "201", "160", "140", "202", "176", "140", "203", "192", "140", "204", "208", "140", "205", "224", "140", "206", "240", "140", "207", "0", "125", "0", "15", "112", "1", "31", "112", "2", "47", "112", "3", "63", "112", "4", "79", "112", "5", "95", "112", "6", "111", "112", "7", "127", "112", "8", "143", "112", "9", "159", "112", "10", "175", "112", "11", "191", "112", "12", "207", "112", "13", "223", "112", "14", "239", "112", "15", "255", "112", "16", "15", "113", "17", "31", "113", "18", "47", "113", "19", "63", "113", "20", "79", "113", "21", "95", "113", "22", "111", "113", "23", "127", "113", "24", "143", "113", "25", "159", "113", "26", "175", "113", "27", "191", "113", "28", "207", "113", "29", "223", "113", "30", "239", "113", "31", "255", "113", "32", "15", "114", "33", "31", "114", "34", "47", "114", "35", "63", "114", "36", "79", "114", "37", "95", "114", "38", "111", "114", "39", "127", "114", "40", "143", "114", "41", "159", "114", "42", "175", "114", "43", "191", "114", "44", "207", "114", "45", "223", "114", "46", "239", "114", "47").map(BigInt(_, 10))
    //doFlowPeekPokeTest("testpce4",CompressDecode.polyComEn4Component(),Seq(testvalue),Goldenen4)
    //doFlowPeekPokeTest("testpce1",CompressDecode.polyComEn1Component(),Seq(testvalue),Goldenen1)
//    doFlowPeekPokeTest("testpvce10",CompressDecode.polyvecComEn10Component(),Seq(testvalue2),Goldenen10)
    //doFlowPeekPokeTest("testpvce12",CompressDecode.polyvecEn12Component(),Seq(testvalue1),Goldenen12)


    val Goldende4 = Seq("0", "0", "208", "0", "416", "0", "624", "0", "832", "0", "1040", "0", "1248", "0", "1456", "0", "1665", "0", "1873", "0", "2081", "0", "2289", "0", "2497", "0", "2705", "0", "2913", "0", "3121", "0", "0", "208", "208", "208", "416", "208", "624", "208", "832", "208", "1040", "208", "1248", "208", "1456", "208", "1665", "208", "1873", "208", "2081", "208", "2289", "208", "2497", "208", "2705", "208", "2913", "208", "3121", "208", "0", "416", "208", "416", "416", "416", "624", "416", "832", "416", "1040", "416", "1248", "416", "1456", "416", "1665", "416", "1873", "416", "2081", "416", "2289", "416", "2497", "416", "2705", "416", "2913", "416", "3121", "416", "0", "624", "208", "624", "416", "624", "624", "624", "832", "624", "1040", "624", "1248", "624", "1456", "624", "1665", "624", "1873", "624", "2081", "624", "2289", "624", "2497", "624", "2705", "624", "2913", "624", "3121", "624", "0", "832", "208", "832", "416", "832", "624", "832", "832", "832", "1040", "832", "1248", "832", "1456", "832", "1665", "832", "1873", "832", "2081", "832", "2289", "832", "2497", "832", "2705", "832", "2913", "832", "3121", "832", "0", "1040", "208", "1040", "416", "1040", "624", "1040", "832", "1040", "1040", "1040", "1248", "1040", "1456", "1040", "1665", "1040", "1873", "1040", "2081", "1040", "2289", "1040", "2497", "1040", "2705", "1040", "2913", "1040", "3121", "1040", "0", "1248", "208", "1248", "416", "1248", "624", "1248", "832", "1248", "1040", "1248", "1248", "1248", "1456", "1248", "1665", "1248", "1873", "1248", "2081", "1248", "2289", "1248", "2497", "1248", "2705", "1248", "2913", "1248", "3121", "1248", "0", "1456", "208", "1456", "416", "1456", "624", "1456", "832", "1456", "1040", "1456", "1248", "1456", "1456", "1456", "1665", "1456", "1873", "1456", "2081", "1456", "2289", "1456", "2497", "1456", "2705", "1456", "2913", "1456", "3121", "1456").map(BigInt(_, 10))
    val Goldende12 = Seq("0", "0", "257", "16", "514", "32", "771", "48", "1028", "64", "1285", "80", "1542", "96", "1799", "112", "2056", "128", "2313", "144", "2570", "160", "2827", "176", "3084", "192", "3341", "208", "3598", "224", "3855", "240", "16", "257", "273", "273", "530", "289", "787", "305", "1044", "321", "1301", "337", "1558", "353", "1815", "369", "2072", "385", "2329", "401", "2586", "417", "2843", "433", "3100", "449", "3357", "465", "3614", "481", "3871", "497", "32", "514", "289", "530", "546", "546", "803", "562", "1060", "578", "1317", "594", "1574", "610", "1831", "626", "2088", "642", "2345", "658", "2602", "674", "2859", "690", "3116", "706", "3373", "722", "3630", "738", "3887", "754", "48", "771", "305", "787", "562", "803", "819", "819", "1076", "835", "1333", "851", "1590", "867", "1847", "883", "2104", "899", "2361", "915", "2618", "931", "2875", "947", "3132", "963", "3389", "979", "3646", "995", "3903", "1011", "64", "1028", "321", "1044", "578", "1060", "835", "1076", "1092", "1092", "1349", "1108", "1606", "1124", "1863", "1140", "2120", "1156", "2377", "1172", "2634", "1188", "2891", "1204", "3148", "1220", "3405", "1236", "3662", "1252", "3919", "1268", "80", "1285", "337", "1301", "594", "1317", "851", "1333", "1108", "1349", "1365", "1365", "1622", "1381", "1879", "1397", "2136", "1413", "2393", "1429", "2650", "1445", "2907", "1461", "3164", "1477", "3421", "1493", "3678", "1509", "3935", "1525", "96", "1542", "353", "1558", "610", "1574", "867", "1590", "1124", "1606", "1381", "1622", "1638", "1638", "1895", "1654", "2152", "1670", "2409", "1686", "2666", "1702", "2923", "1718", "3180", "1734", "3437", "1750", "3694", "1766", "3951", "1782", "112", "1799", "369", "1815", "626", "1831", "883", "1847", "1140", "1863", "1397", "1879", "1654", "1895", "1911", "1911", "2168", "1927", "2425", "1943", "2682", "1959", "2939", "1975", "3196", "1991", "3453", "2007", "3710", "2023", "3967", "2039", "128", "2056", "385", "2072", "642", "2088", "899", "2104", "1156", "2120", "1413", "2136", "1670", "2152", "1927", "2168", "2184", "2184", "2441", "2200", "2698", "2216", "2955", "2232", "3212", "2248", "3469", "2264", "3726", "2280", "3983", "2296", "144", "2313", "401", "2329", "658", "2345", "915", "2361", "1172", "2377", "1429", "2393", "1686", "2409", "1943", "2425", "2200", "2441", "2457", "2457", "2714", "2473", "2971", "2489", "3228", "2505", "3485", "2521", "3742", "2537", "3999", "2553", "160", "2570", "417", "2586", "674", "2602", "931", "2618", "1188", "2634", "1445", "2650", "1702", "2666", "1959", "2682", "2216", "2698", "2473", "2714", "2730", "2730", "2987", "2746", "3244", "2762", "3501", "2778", "3758", "2794", "4015", "2810", "176", "2827", "433", "2843", "690", "2859", "947", "2875", "1204", "2891", "1461", "2907", "1718", "2923", "1975", "2939", "2232", "2955", "2489", "2971", "2746", "2987", "3003", "3003", "3260", "3019", "3517", "3035", "3774", "3051", "4031", "3067", "192", "3084", "449", "3100", "706", "3116", "963", "3132", "1220", "3148", "1477", "3164", "1734", "3180", "1991", "3196", "2248", "3212", "2505", "3228", "2762", "3244", "3019", "3260", "3276", "3276", "3533", "3292", "3790", "3308", "4047", "3324", "208", "3341", "465", "3357", "722", "3373", "979", "3389", "1236", "3405", "1493", "3421", "1750", "3437", "2007", "3453", "2264", "3469", "2521", "3485", "2778", "3501", "3035", "3517", "3292", "3533", "3549", "3549", "3806", "3565", "4063", "3581", "224", "3598", "481", "3614", "738", "3630", "995", "3646", "1252", "3662", "1509", "3678", "1766", "3694", "2023", "3710", "2280", "3726", "2537", "3742", "2794", "3758", "3051", "3774", "3308", "3790", "3565", "3806", "3822", "3822", "4079", "3838", "240", "3855", "497", "3871", "754", "3887", "1011", "3903", "1268", "3919", "1525", "3935", "1782", "3951", "2039", "3967", "2296", "3983", "2553", "3999", "2810", "4015", "3067", "4031", "3324", "4047", "3581", "4063", "3838", "4079", "4095", "4095").map(BigInt(_, 10))
    val Goldende1 = Seq("0", "0", "0", "0", "0", "0", "0", "0", "1665", "0", "0", "0", "0", "0", "0", "0", "0", "1665", "0", "0", "0", "0", "0", "0", "1665", "1665", "0", "0", "0", "0", "0", "0", "0", "0", "1665", "0", "0", "0", "0", "0", "1665", "0", "1665", "0", "0", "0", "0", "0", "0", "1665", "1665", "0", "0", "0", "0", "0", "1665", "1665", "1665", "0", "0", "0", "0", "0", "0", "0", "0", "1665", "0", "0", "0", "0", "1665", "0", "0", "1665", "0", "0", "0", "0", "0", "1665", "0", "1665", "0", "0", "0", "0", "1665", "1665", "0", "1665", "0", "0", "0", "0", "0", "0", "1665", "1665", "0", "0", "0", "0", "1665", "0", "1665", "1665", "0", "0", "0", "0", "0", "1665", "1665", "1665", "0", "0", "0", "0", "1665", "1665", "1665", "1665", "0", "0", "0", "0", "0", "0", "0", "0", "1665", "0", "0", "0", "1665", "0", "0", "0", "1665", "0", "0", "0", "0", "1665", "0", "0", "1665", "0", "0", "0", "1665", "1665", "0", "0", "1665", "0", "0", "0", "0", "0", "1665", "0", "1665", "0", "0", "0", "1665", "0", "1665", "0", "1665", "0", "0", "0", "0", "1665", "1665", "0", "1665", "0", "0", "0", "1665", "1665", "1665", "0", "1665", "0", "0", "0", "0", "0", "0", "1665", "1665", "0", "0", "0", "1665", "0", "0", "1665", "1665", "0", "0", "0", "0", "1665", "0", "1665", "1665", "0", "0", "0", "1665", "1665", "0", "1665", "1665", "0", "0", "0", "0", "0", "1665", "1665", "1665", "0", "0", "0", "1665", "0", "1665", "1665", "1665", "0", "0", "0", "0", "1665", "1665", "1665", "1665", "0", "0", "0", "1665", "1665", "1665", "1665", "1665", "0", "0", "0").map(BigInt(_, 10))
    val Goldende10 = Seq("0", "0", "52", "13", "1668", "416", "104", "39", "2507", "832", "208", "52", "849", "1044", "312", "78", "2516", "1460", "364", "104", "26", "1879", "468", "117", "1697", "2087", "572", "143", "36", "2507", "624", "169", "875", "2923", "728", "182", "2546", "3131", "832", "208", "884", "221", "888", "234", "1723", "637", "992", "247", "65", "849", "1096", "273", "1733", "1265", "1148", "299", "2572", "1681", "1252", "312", "914", "1892", "1356", "338", "2581", "2308", "1408", "364", "91", "2728", "1512", "377", "1762", "2936", "1616", "403", "101", "26", "1671", "429", "940", "442", "1775", "442", "2611", "650", "1879", "468", "949", "1070", "1931", "494", "1788", "1486", "2035", "507", "130", "1697", "2139", "533", "1798", "2113", "2191", "559", "2637", "2529", "2295", "572", "979", "2741", "2399", "598", "2646", "3157", "2451", "624", "156", "247", "2559", "637", "1827", "455", "2663", "663", "166", "875", "2715", "689", "1005", "1291", "2819", "702", "2676", "1499", "2923", "728", "1014", "1918", "2975", "754", "1853", "2334", "3079", "767", "195", "2546", "3183", "793", "1863", "2962", "3235", "819", "2702", "49", "13", "836", "1044", "260", "117", "862", "2711", "676", "169", "888", "221", "1096", "273", "901", "1892", "1304", "377", "927", "231", "1723", "429", "953", "1070", "2139", "533", "966", "2741", "2347", "637", "992", "1079", "2767", "689", "1018", "1918", "3183", "793", "1031", "260", "65", "901", "1057", "1928", "481", "953", "1083", "2767", "897", "1057", "1096", "1109", "1109", "1161", "1122", "2776", "1525", "1213", "1148", "286", "1944", "1317", "1161", "1957", "2152", "1421", "1187", "296", "2572", "1473", "1213", "1135", "2988", "1577", "1226", "2806", "3196", "1681", "1252", "1144", "286", "1736", "1278", "1983", "702", "1840", "1291", "325", "914", "1944", "1317", "1993", "1330", "1996", "1343", "2832", "1746", "2100", "1356", "1174", "1957", "2204", "1382", "2841", "2373", "2256", "1408", "351", "2793", "2360", "1421", "2022", "3001", "2464", "1447", "361", "91", "2520", "1473", "1200", "507", "2624", "1486", "2871", "715", "2728", "1512", "1209", "1135", "2780", "1538", "2048", "1551", "2884", "1551", "390", "1762", "2988", "1577", "2058", "2178", "3040", "1603", "2897", "2594", "3144", "1616", "1239", "2806", "3248", "1642", "2906", "3222", "3300", "1668", "416", "312", "78", "1684", "2087", "520", "182", "1710", "426", "940", "234", "1736", "1265", "1356", "338", "1749", "2936", "1564", "442", "1775", "1274", "1983", "494", "1801", "2113", "2399", "598", "1814", "455", "2611", "702", "1840", "2123", "3027", "754", "1866", "2962", "114", "862", "1879", "1304", "325", "966", "1905", "2971", "741", "1018", "1931", "481", "1161", "1122", "1944", "2152", "1369", "1226", "1970", "491", "1788", "1278", "1996", "1330", "2204", "1382", "2009", "3001", "2412", "1486", "2035", "1339", "2832", "1538", "2061", "2178", "3248", "1642", "2074", "520", "130", "1749", "2100", "2188", "546", "1801", "2126", "3027", "962", "1905", "2139", "1369", "1174", "2009", "2165", "3036", "1590", "2061", "2191", "546", "2009", "2165", "2204", "2217", "2217", "2269", "2230", "556", "2637", "2321", "2256", "1395", "3053", "2425", "2269", "3066", "3261", "2529", "2295", "1404", "351", "2585", "2321", "2243", "767", "2689", "2334", "585", "979", "2793", "2360", "2253", "1395", "2845", "2386", "3092", "1811", "2949", "2399", "1434", "2022", "3053", "2425", "3101", "2438", "3105", "2451", "611", "2858", "3209", "2464", "2282", "3066", "3313", "2490", "621", "156", "39", "2520", "1460", "572", "143", "2533", "3131", "780", "247", "2559", "1469", "1200", "299", "2585", "2308", "1616", "403", "2598", "650", "1827", "507", "2624", "2318", "2243", "559", "2650", "3157", "2659", "663", "2663", "1499", "2871", "767", "2689", "3166", "3287", "819", "2715", "676", "377", "927", "2728", "2347", "585", "1031", "2754", "686", "1005", "1083", "2780").map(BigInt(_, 10))
    //doFlowPeekPokeTest("testDecode12",CompressDecode.Decode12(),Seq(testvalueBits12),Goldende12)
    //doFlowPeekPokeTest("testpvcde12",CompressDecode.polyvecDe12Component(),Seq(testvalueBits12),Goldende12)
    //doFlowPeekPokeTest("testpde1",CompressDecode.polyDe1Component(),Seq(testvalueBits1),Goldende1)
    //doFlowPeekPokeTest("testpde4",CompressDecode.polyDe4Component(),Seq(testvalueBits),Goldende4)
    //doFlowPeekPokeTest("testpvcde10",CompressDecode.polyvecDe10Component(),Seq(testvalueBits10),Goldende10)

//    VivadoSynth(CompressDecode.ComEncode1(),"En1")
    val before = Seq("144","1868","1801","524","1767","2443","683","2080","1428","2089","3223","1444","3274","797","2841","2819","1324","2012","3140","1993","100","711","3155","3321","812","2117","1530","1756","3277","844","1037","1940","3293","255","3308","2184","2130","57","1590","3250","1795","2230","748","1209","2872","2004","137","1636","2777","1507","937","3178","2066","771","3157","3027","2889","1405","2614","1088","288","3127","2908","652","2134","1862","783","2290","2198","83","28","678","535","1818","2643","2002","514","872","658","1096","2760","1275","238","2141","2609","1099","2298","2043","169","285","162","1934","1008","2363","1438","1529","1068","67","586","1461","3201","234","525","1069","2483","2851","1065","923","110","1485","2268","1414","1557","1094","2146","3318","577","7","341","1587","95","140","425","663","692","1776","1100","3159","284","2148","2151","1459","1844","1700","707","2736","344","999","1071","1448","2723","2090","602","1154","541","2373","3238","1582","3194","512","1332","3266","361","1915","29","595","3016","121","1792","1980","770","408","2391","2777","226","2945","1321","2337","3237","988","2633","2896","2236","1488","2718","2920","2788","2537","2420","2091","2337","1389","174","933","2124","1882","3155","684","2976","2249","1794","2519","1417","2795","2863","94","445","345","424","1712","2802","420","3009","3222","1483","3207","464","1892","159","2249","1080","1085","2635","743","138","1564","3210","2626","377","1354","91","278","1738","1614","998","2977","2405","799","2221","2063","1795","1149","590","1213","3280","1531","19","2795","2739","2714","1119","125","1182","3273","1523","1988","884","671","3150","56","502","1476","3075","232","1749","752").map(BigInt(_,10))
    val compress10 = Seq(
      "1181","1003","2190","3070","1319","2364","3261","1375","1521","2936","3253","435","3046","1328","2758","558","1660","3134","3101","772","2681","283","1021","1844","2167","968","1750","935","1247","578","2067","2981","2023","3011","1007","1036","54","2360","2734","1884","2566","2184","1472","925","962","1263","1319","1389","1507","2980","333","462","1178","1259","496","2021","326","1313","1709","1284","2093","3306","1376","974","1249","570","986","2274","1728","2646","2141","1591","2496","438","2680","777","487","3216","1755","1264","1373","813","1582","1193","1769","3149","234","2376","945","632","2388","649","2867","647","2630","1501","2381","515","1914","572","1259","1697","2435","2677","773","312","389","1178","2714","1732","2007","1340","843","1101","1870","313","2457","2101","1341","2599","2494","425","2642","1861","2185","2031","1681","1897","1275","511","1638","2072","3150","814","1988","956","35","2431","3144","2665","2115","1114","2219","1822","573","2392","3318","814","1719","598","2551","929","2296","1022","1727","687","2576","2014","103","725","1891","3147","1019","232","907","2372","1436","2530","2374","521","1485","791","2263","3049","2253","544","1398","1790","1347","2574","1142","567","255","1594","1718","1513","3259","1545","1117","2001","1234","784","2303","1236","3","1637","2625","1913","1067","1366","265","1378","2242","2193","2320","2771","1749","2106","3315","1122","899","779","904","2082","2162","3020","2461","2503","679","150","2073","664","903","2772","3143","2029","130","272","99","2710","1326","2715","848","880","3233","1528","2374","1855","2326","1018","744","3089","1982","499","1124","656","880","1000","1516","1424","2225","357","2935","207","2527","2452","185","1320","2879","2703","2140","730","129","1817","2872","1376","1623","2705","601","1866","750","2501","2953","896","2646","3241","2903","2315","2933","3235","1499","493","3162","90","1365","307","965","1740","2061","323","2719","2033","1792","338","2343","191","2954","792","3210","1782","225","1389","936","2243","1188","1970","1618","2754","530","530","2669","1512","1467","402","967","1159","59","1082","1039","798","2058","1079","2434","3158","937","1762","1205","2441","2675","1513","973","1513","2311","1860","422","2201","1401","2555","2456","1124","1891","1926","469","107","224","772","1764","1433","2950","938","1999","295","6","2032","1278","3323","465","2753","2941","525","2699","270","3190","3226","675","2440","1801","1499","2309","17","2927","1034","3151","471","1954","1256","3267","3308","434","814","336","485","1863","2740","2044","543","1819","2264","3060","2617","665","207","2637","457","1213","840","3328","413","649","1959","2659","465","2492","2128","1017","1589","1436","1724","3230","2079","1051","73","198","2002","254","1789","873","167","2396","2585","2988","1726","1786","1920","2643","2788","1764","1450","2969","1660","2702","1021","1092","783","2906","1701","2309","1832","1435","1023","1292","1853","843","73","71","2902","2930","1705","2875","1391","1050","1070","410","89","745","1036","303","1164","961","1770","2891","565","531","1693","1377","1227","2554","2442","2926","2085","779","1875","2257","2215","2461","2600","1606","1385","977","2153","1662","13","1426","1162","664","580","3064","809","2392","3235","3267","1176","1322","2891","1750","2918","1694","2115","1486","1779","1261","2347","2687","2779","1755","3233","981","166","1443","2353","1313","1040"
    ).map(BigInt(_,10))


    val enc1=Seq(
      "69d","2a","cc0","9a","18","647","18","cc8","20","a","632","cdf","c5a","6df","53","c91","68a","670","685","63","cf8","6d4","65","26","c7b","6b0","cdf","6b0","c95","67a","c89","cda","6b4","38","720","654","cee","6b1","5d","12","ce2","ccd","53","16","622","625","38","c9e","6ba","68d","cdc","63","6e9","715","cc8","c5f","ce6","6a1","66a","8","67f","607","47","ca2","6f7","cfc","95","695","6c3","66e","cba","cc8","20","c66","687","65a","6a5","6f7","44","ca3","672","69f","677","68e","5f8","695","cd3","9a","cd1","6a4","7e","a","7a","55","658","cf6","66a","c55","6c4","cd8","cec","77","5fe","c","19","30","cac","621","34","b7","6d2","6d","661","70c","cc4","659","11","6f","5e8","55","cb8","6ba","6c1","6de","6d","ce5","6ff","cfc","679","33","cae","ce7","657","ceb","610","27","cfd","32","6ae","16","667","c65","6ac","3","643","600","6ab","44","69d","cf0","6b5","cf6","b6","654","63","686","69c","cbd","65e","47","68f","18","692","616","654","ce8","6a7","67","48","cd3","4c","cb1","93","6c7","653","cf8","6ea","625","cb6","a","cb2","656","666","cb6","23","6b5","6a2","27","c68","65c","697","3e","743","2c","2a","6c8","cdd","684","6c9","19","8a","c74","6ed","6b5","d00","632","672","3f","68b","66f","63a","6ca","27","6de","6b7","59","cd6","66d","49","cf5","691","6b0","64d","58","68c","c59","63d","ce5","6c5","697","65d","5c","cd8","b","c6a","60a","654","717","6f0","94","68b","6ef","72","686","68d","654","65d","11","cc0","67c","67f","666","65f","6c0","6fe","0"
    ).map(BigInt(_,16))

    SimConfig.withFstWave.compile(new CompressDecode.Compress1(10)).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      var idx = 0
      dut.dataIn.valid#=false

      var i =0
      sleep(1)
      while(i<1000)
      {
        if(i<100){
          dut.dataIn.valid #=true
          dut.dataIn.payload #=enc1(i)
        }
        else{
          dut.dataIn.valid #= false
        }

        i= i+1
        sleep(10)
      }

    }



    val deCompress = Seq(
      "625","170","920","32","891","541","827","267","197","79","623","617","935","651","650","649","561","536","272","994","397","817","873","612","808","459","326","731","100","481","935","735","140","487","1013","930","469","590","75","284","839","949","549","811","564","139","407","155","828","743","30","401","945","179","843","221","600","763","991","1003","278","184","724","394","981","553","192","237","544","342","424","580","795","601","878","962","444","904","609","131","717","658","590","682","633","416","755","437","944","57","720","962","119","961","690","292","851","1001","957","554","941","756","635","94","949","560","204","811","494","476","704","556","1009","394","940","569","751","622","434","710","974","689","703","476","862","133","345","205","1004","18","1021","820","838","658","475","538","201","22","498","182","504","307","632","788","1018","334","54","650","865","346","645","762","907","10","997","633","730","293","676","354","418","415","617","193","171","494","851","238","274","953","10","834","511","183","774","769","604","12","580","926","537","585","883","556","201","179","132","964","775","389","903","890","78","363","943","220","743","61","165","348","861","243","540","902","440","367","820","557","424","243","819","930","148","765","616","415","540","454","275","832","903","834","94","367","381","173","406","897","622","863","764","815","251","122","630","51","99","516","787","50","114","895","992","114","508","906","443","186","545","908","613","217","6","392","656","862","517","707","647","894","321","322","75","820","277","259","276","878","720","436","208","478","934","1018","597","712","392","950","863","760","880","145","453","304","108","608","698","420","86","497","445","141","437","361","718","520","544","834","836","68","915","147","416","493","443","629","631","494","955","881","913","179","890","873","61","971","465","554","237","786","294","595","441","352","156","236","295","950","658","188","162","204","100","493","419","829","940","63","500","218","381","981","997","177","628","1002","664","819","683","690","36","704","687","125","314","48","439","511","940","905","372","913","916","815","546","974","404","316","43","896","258","946","198","313","347","178","151","176","986","658","129","234","718","623","725","786","547","325","966","574","442","384","668","441","931","450","484","785","719","725","42","385","381","381","341","612","466","556","727","781","174","457","676","373","484","45","316","729","659","830","257","532","28","1020","689","239","502","691","453","36","851","43","136","906","341","732","839","700","944","10","860","540","718","440","83","343","395","708","866","131","393","650","205","312","923","922","465","939","95","172","470","593","955","796","784","427","386","942","222","750","739","373","152","717","173","710","679","187","111","189","945","920","817","947","69","481","970","681","297","752","147","843","995","431","496","198","609","501","276","108","771","677","960","319","748","759","857","720","693","629","870","882"
    ).map(BigInt(_,10))
    val dec1=Seq(
      "2a272421","3633302d","423f3c39","4e4b4845","5a575451","6663605d","726f6c69","7e7b7875"
    ).map(BigInt(_,16))


//    VivadoSynth(CompressDecode.BAR())
//    VivadoSynth(CompressDecode.CompressDu(10))


    SimConfig.withFstWave.compile(new CompressDecode.BAR()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      var idx = 0
      dut.dataIn.valid#=false

      var i =0
      sleep(1)
      while(i<1000)
      {
        if(i<100){
          dut.dataIn.valid #=true
          dut.dataIn.payload #=enc1(i)<<10
        }
        else{
          dut.dataIn.valid #= false
        }

        i= i+1
        sleep(10)
      }

    }


    SimConfig.withFstWave.compile(new CompressDecode.encode12()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.clockDomain.waitSampling()
      var idx = 0
      dut.dataIn.valid#=false

      var i =0
      sleep(1)
      while(i<1000)
      {
        if(i<9){
          dut.dataIn.valid #=true
          dut.dataIn.payload #=(enc1(i+1)<<12) + enc1(i)
        }
        else{
          dut.dataIn.valid #= false
        }

        i= i+1
        sleep(10)
      }

    }
//    SimConfig.withFstWave.compile(new CompressDecode.ComEncode1()).doSim { dut =>
//      dut.clockDomain.forkStimulus(10)
//      dut.clockDomain.waitSampling()
//      var idx = 0
//      dut.dataIn.valid#=false
//
//
//      sleep(1)
//      while(idx<1000)
//      {
//        if(idx<256){
//          dut.dataIn.valid #= true
//          dut.dataIn.payload #= (enc1(idx+1)<<12) + enc1(idx)
//
//        }
//        else{
//          dut.dataIn.valid #= false
//        }
//        idx += 2
//        sleep(10)
//      }
//
//    }
    val mIn =Seq("2a272421","3633302d","423f3c39","4e4b4845","5a575451","6663605d","726f6c69","7e7b7875"
    ).map(BigInt(_,16))
//    SimConfig.withFstWave.compile(new CompressDecode.deComDecode1()).doSim { dut =>
//            dut.clockDomain.forkStimulus(10)
//            dut.clockDomain.waitSampling()
//            var idx = 0
//            dut.dataIn.valid#=false
//      dut.work#=false
//            var i =0
//            sleep(1)
//            while(i<1000)
//            {
//              if(i<8){
//                dut.dataIn.valid #= true
//                dut.dataIn.payload #=mIn(i)
//              }
//              else if(i>20)(dut.work#=true)
//              else{
//                dut.dataIn.valid #= false
//              }
//              i= i+1
//             sleep(10)
//            }
//
//    }
  }
}
