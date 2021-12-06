package Chainsaw.crypto.kyber

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.kyber.Reduce.csubq
import Chainsaw.crypto.kyber._

import Chainsaw.crypto.symmetric._
import Chainsaw.dspTest._
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
  //Poly_toBytes

  //without compress
  //todo test failed
  def polyEn12(A: Array[SInt]): Array[Bits] = {
    require((A.forall(_.getWidth == 16)) && A.size == 256)
    val Cs = A.map(a => csubq(a).asBits)
    val Ds = Cs.grouped(2).toArray
    Ds.map(d => (d(1).takeLow(8) ## ((d(1)>>8).takeLow(8) | (d(0)<<4).takeLow(8)) ## (d(0) >> 4).takeLow(8)).subdivideIn(8 bits).toArray).flatten
  }

  //todo verify

  // In 384*8 Out Polyvec:2*256*16
  //Decode12
  //polyvec_frombytes in unpack_ciphertext
  def polyDe12(A: Array[Bits]): Array[Array[SInt]] = {
    val Bs = A.reduce(_ ## _).reversed.subdivideIn(12 bits).toArray.map(B(_, 16 bits).asSInt)
    Bs.grouped(256).toArray
  }

  //IN 640*8 Out: ployvec 2*256816
  //Decompress(Decode10,10)
  //poly_decompress in unpack_ciphertext
  def ployvecDecompressDe10(A: Array[Bits]): Array[Array[SInt]] = {
    val Bs: Array[SInt] = A.reduce(_ ## _).subdivideIn(10 bits).toArray.map(_.asSInt)
    val Cs: Array[SInt] = Bs.map(c => (c * q + 512) >> 10).map(B(_, 16 bits).asSInt)
    Cs.grouped(256).toArray
  }

  //In: 128*8 Out: poly 256*16
  //Decompress(Decode4,4)
  //poly_decompress in unpack_ciphertext
  def ployDecompressDe4(A: Array[Bits]): Array[SInt] = {
    val Bs: Array[SInt] = A.reduce(_ ## _).subdivideIn(4 bits).toArray.map(_.asSInt)
    Bs.map(c => (c * q + 8) >> 4).map(B(_, 16 bits).asSInt)

  }

  //In:ploy  Out:32*8
  //compress(encode1,1)
  //poly_frommsg
  def ployDecompressDe1(A: Array[Bits]): Array[SInt] = {
    val Bs = A.reduce(_ ## _).subdivideIn(1 bits).toArray.map(_.asUInt)
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
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(16 bits), 256)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 384)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyEn12(dataIn.payload.toArray.map(_.asSInt))))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

}

object TestCompressDecode {
  def main(args: Array[String]): Unit = {
        val testvalue = Seq.fill(256)(0).zipWithIndex.map{
          case(i,kk) => BigInt(kk*128)
        }
        val testvalue2=Seq.fill(512)(0)
          .zipWithIndex.map{
          case(i,kk) => BigInt(kk*64+256)
        }

    val Goldenen4 = Seq("16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "101", "118", "135", "153", "170", "203", "220", "238", "255", "16", "33", "50", "68", "85", "118", "135", "153", "170", "203", "220", "237", "255", "16", "33", "50", "68", "85", "118", "135", "153", "170", "203", "220").map(BigInt(_, 10))
    val Goldenen1 =Seq("128","255","15","0","254","63","0","248","255","0","224","255","3","128","255","15","0","254","63","0","248","255","0","224","255","3","128","255","15","0","254","63").map(BigInt(_,10))
    val Goldenen10 = Seq("79","136","97","135","34","157","196","82","76","54","236","0","68","209","73","59","61","37","150","93","138","117","22","91","113","216","177","7","32","133","39","238","248","164","152","118","42","218","105","172","197","98","203","46","192","19","159","188","243","211","98","219","173","120","231","177","23","143","61","251","0","76","112","194","14","78","136","97","71","34","157","196","82","12","54","236","0","52","209","73","59","57","37","150","93","137","117","22","27","113","216","177","7","224","132","39","238","232","164","152","118","38","218","105","172","196","98","203","238","191","19","159","188","179","211","98","219","157","120","231","177","19","143","61","251","255","79","112","130","14","78","136","97","71","34","157","196","66","12","54","236","252","51","209","73","58","57","37","86","93","137","117","6","27","113","216","177","247","223","132","39","234","232","164","152","117","38","218","41","172","196","98","187","238","191","19","159","172","179","211","98","215","157","120","231","176","19","143","253","250","255","79","96","130","14","78","136","81","71","34","157","192","66","12","54","235","252","51","145","73","58","57","21","86","93","137","113","6","27","113","216","173","247","223","132","38","234","232","100","152","117","38","202","41","172","196","94","187","238","191","19","155","172","179","211","97","215","157","56","231","176","19","127","253","250","255","75","96","130","14","78","132","81","71","34","156","192","66","204","53","235","252","35","145","73","58","53","21","86","93","136","113","6","27","113","215","173","247","159","132","38","234","216","100","152","117","34","202","41","172","195","94","187","238","191","18","155","172","115","211","97","215","141","56","231","176","15","127","253","250","254","75","96","130","14","77","132","81","7","34","156","192","50","204","53","235","248","35","145","73","57","53","21","86","93","136","113","6","219","112","215","173","231","159","132","38","230","216","100","152","116","34","202","233","171","195","94","187","174","191","18","155","156","115","211","97","211","141","56","231","175","15","127","189","250","254","75","96","66","14","77","132","65","7","34","156","188","50","204","53","234","248","35","81","73","57","53","21","22","93","136","113","246","218","112","215","169","231","159","132","37","230","216","36","152","116","34","186","233","171","195","94","171","174","191","18","151","156","115","211","96","211","141","248","230","175","15","111","189","250","254","75","80","66","14","77","128","65","7","34","155","188","50","140","53","234","248","19","81","73","57","53","5","22","93","136","109","246","218","112","214","169","231","95","132","37","230","200","36","152","116","30","186","233","171","195","90","171","174","191","17","151","156","51","211","96","211","125","248","230","175","11","111","189","250","254","71","80","66","14","76","128","65","199","33","155","188","34","140","53","234","244","19","81","73","57","49","5","22","93","135","109","246","154","112","214","169","215","95","132","37","226","200","36","152","116","30","186","233","171","194","90","171","110","191","17","151","140","51","211","96","207","125","248","230","174","11","111","189","250","253","71","80","2","14","76","128","49","199","33","155","184","34","140","53","233","244","19","81","73","56","49","5","214","92","135","109","230","154","112","214","165","215","95","132","36","226","200","36","152","115","30","186","169","171","194","90","155","110","191","17","147","140","51","211","95","207","125","184","230").map(BigInt(_, 10))
    val Goldenen12 =Seq("0","0","8","0","1","24","0","2","40","0","3","56","0","4","72","0","5","88","0","6","104","0","7","120","0","8","136","0","9","152","0","10","168","0","11","184","0","12","200","0","253","7","255","240","23","255","241","39","255","242","55","255","243","71","255","244","87","255","245","103","255","246","119","255","247","135","255","248","151","255","249","167","255","250","183","255","251","199","255","236","7","254","224","23","254","225","39","254","226","55","254","227","71","254","228","87","254","229","103","254","230","119","254","231","135","254","232","151","254","233","167","254","234","183","254","235","199","254","220","7","253","208","23","253","209","39","253","210","55","253","211","71","253","212","87","253","213","103","253","214","119","253","215","135","253","216","151","253","217","167","253","218","183","253","219","199","253","220","215","253","221","231","253","222","247","253","223","7","253","208","23","253","209","39","253","210","55","253","211","71","253","212","87","253","213","103","253","214","119","253","215","135","253","216","151","253","217","167","253","218","183","253","219","199","253","220","215","253","221","231","253","222","247","253","223","7","253","240","23","253","241","39","253","242","55","253","243","71","253","244","87","253","245","103","253","246","119","253","247","135","253","248","151","253","249","167","253","250","183","253","251","199","253","252","215","253","253","231","253","254","247","253","255","7","253","240","23","253","241","39","253","242","55","253","243","71","253","244","87","253","245","103","253","246","119","253","247","135","253","248","151","253","249","167","253","250","183","253","251","199","253","252","215","253","253","231","253","254","247","253","255","7","253","208","23","253","209","39","253","210","55","253","211","71","253","212","87","253","213","103","253","214","119","253","215","135","253","216","151","253","217","167","253","218","183","253","219","199","253","220","215","253","221","231","253","222","247","253","223","7","253","208","23","253","209","39","253","210","55","253","211","71","253","212","87","253","213","103","253","214","119","253","215","135").map(BigInt(_,10))
    //doFlowPeekPokeTest("testpce4",CompressDecode.polyComEn4Component(),Seq(testvalue),Goldenen4)
    //doFlowPeekPokeTest("testpvce1",CompressDecode.polyComEn1Component(),Seq(testvalue),Goldenen1)
    //doFlowPeekPokeTest("testpvce10",CompressDecode.polyvecComEn10Component(),Seq(testvalue2),Goldenen10)
    //doFlowPeekPokeTest("testpvce12",CompressDecode.polyvecEn12Component(),Seq(testvalue),Goldenen12)

  }
}
