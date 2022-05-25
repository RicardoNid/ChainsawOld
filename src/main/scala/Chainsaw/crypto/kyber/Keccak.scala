package Chainsaw.crypto.kyber

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.kyber._
import Chainsaw.crypto.symmetric._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

object Keccak {

  def kState: Array[Bits] = Array(
    "0000000000000001",
    "0000000000008082",
    "800000000000808a",
    "8000000080008000",
    "000000000000808b",
    "0000000080000001",
    "8000000080008081",
    "8000000000008009",
    "000000000000008a",
    "0000000000000088",
    "0000000080008009",
    "000000008000000a",
    "000000008000808b",
    "800000000000008b",
    "8000000000008089",
    "8000000000008003",
    "8000000000008002",
    "8000000000000080",
    "000000000000800a",
    "800000008000000a",
    "8000000080008081",
    "8000000000008080",
    "0000000080000001",
    "8000000080008008"
  ).map(BigInt(_, 16)).map(B(_, 64 bits))

  def nroundState = Array("01","1A","5E","70","1F","21","79","55","0E","0C","35","26","3F","4F","5D","53","52","48","16","66","79","58","21","74").map(BigInt(_,16)).map(B(_,7 bits))

  //val rXYValue = Seq(0, 1, 62, 28, 27, 36, 44, 6, 55, 20, 3, 10, 43, 25, 39, 41, 45, 15, 21, 8, 18, 2, 61, 56, 14)
  val rXYValue = Seq(0, 36, 3, 41, 18, 1, 44, 10, 45, 2, 62, 6, 43, 15, 61, 28, 55, 25, 21, 56, 27, 20, 39, 8, 14)

  def theta(A: Array[Array[Bits]]): Array[Array[Bits]] = {
    require(A.forall(_.forall(_.getBitsWidth == 64))) // check bit width
    // theta1, row-wise reduce XOR
    val B: Array[Bits] = A.map(_.reduce(_ ^ _))
    //theta2
    val C = Array.tabulate(5)(x => B((x + 4) % 5) ^ B((x + 1) % 5).rotateLeft(1))
    //theta3
    A.zip(C).map { case (a, c) => a.map(_ ^ c) }
  }

  def rou(A: Array[Array[Bits]]): Array[Array[Bits]] =
    A.flatten.zip(rXYValue).map { case (bits, i) => bits.rotateLeft(i) }.grouped(5).toArray

  def pi(A: Array[Array[Bits]]): Array[Array[Bits]] =
    Array.tabulate(5, 5)((x, y) => A((x + 3 * y) % 5)(x))

  //def rouPi(A: Array[Array[Bits]]): Array[Array[Bits]] =

  def chi(A: Array[Array[Bits]]): Array[Array[Bits]] =
    Array.tabulate(5, 5)((x, y) => A(x)(y) ^ (~A((x + 1) % 5)(y) & A((2 + x) % 5)(y)))

  def iota(A: Array[Array[Bits]], state: Bits): Array[Array[Bits]] =
    ((A.flatten.head ^ state) +: A.flatten.tail).grouped(5).toArray


  def iotaReplac(A: Array[Array[Bits]], state: Bits): Array[Array[Bits]] = {
//    var aWithIndex: Array[Bits] =A.flatten.head.subdivideIn(1 bits).toArray
    //val aaaa = Array[Bits](aWithIndex.)
//    aWithIndex.foreach{
//      i => aWithIndex(i) = aWithIndex(i)^state(i).asBits
//    }
//    val aa = aWithIndex.reverse.reduce(_##_)

//    loction.update()
//    b.zip()
//    val aindex = 0 until 64
//    val b = ArrayBuffer[Bits]()
//    val bitLoction= Array(0,1,3,7,15,31,63).foreach(i=> b+= aWithIndex(i))
//    val c= (b.reduce(_ ## _) ^ state).subdivideIn(1 bits)

    //.zip(aindex).filter((a,l)=>b += aWithIndex(a))
    val a = A(0)(0)
    val b = (0 until 64).map{ kki=>
      if(kki==0)(a(kki)^state(0))
      else if(kki==1)(a(kki)^state(1))
      else if(kki==3)(a(kki)^state(2))
      else if(kki==7)(a(kki)^state(3))
      else if(kki==15)(a(kki)^state(4))
      else if(kki==31)(a(kki)^state(5))
      else if(kki==63)(a(kki)^state(6))
      else a(kki)
    }.asBits

    (b +: A.flatten.tail).grouped(5).toArray

  }

  //XOR with 0 1 3 7 15 31 63

  def keccakF(state: Array[Bits]): Array[Bits] = {
    require(state.forall(_.getBitsWidth == 64) && state.size == 25)
    val statex: Array[Bits] = (state.grouped(5).toArray).transpose.flatten
    //val statex: Array[Bits] =Array.tabulate(5,5)((x, y)=>stateT(y)(x)).flatten
    val AForEachRound = ArrayBuffer[Array[Bits]](statex)

    (0 until 24).foreach { i =>
      val A0: Array[Array[Bits]] = AForEachRound.last.grouped(5).toArray
      val A1 = theta(A0)
      val A2 = rou(A1)
      val A3 = pi(A2)
      val A4 = chi(A3)
      val A5 = iota(A4,kState(i))
      val ret = A5.flatten
      //A5.transpose.flatten.zipWithIndex.foreach { case (bits, j) => bits.setName(s"round_${i}_${j}") }
      AForEachRound += A5.flatten
    }
    AForEachRound.last.grouped(5).toArray.transpose.flatten
  }

  //Absorb
  def keccakAbsorb(r: Int, m: Array[Bits], mlen: Int, p: Bits): Array[Bits] = {
    //require(m.forall(_.getBitsWidth == 8) && (p.getBitsWidth == 8))
    val s0 = Array.fill(25)(BigInt(0)).map(B(_, 64 bits))
    val C: Array[Bits] = m.take((mlen/8)*8).grouped(8).map(_.reverse.reduce(_ ## _)).toArray //8bytes
    val s = ArrayBuffer[Array[Bits]](s0)
    var ml = mlen
    var ii=0
    while(ml>r){
      val s1: Array[Bits] =(0 until s0.length).map{ kk=>
        if (kk < r/8){
          s.last(kk)^C( ii + kk)
        }
        else (s.last(kk))
      }.toArray
      val s2: Array[Bits] = keccakF(s1)
      s += s2
      ml = ml -r
      ii= ii + r/8
    }
    val t: Seq[Bits] =(0 until 200).map{ i=>
      if(i < ml)( m(i + (mlen/r)*r) )
      else if(i == ml)(B(p,8 bits))
      else if(i==r-1)(B(128,8 bits))
      else (B(0,8 bits))
    }

    val t64 = t.grouped(8).map(_.reverse.reduce(_ ## _)).toArray
    (0 until 25).map{ i=>
      if(i< r/8) s.last(i) ^ t64(i)
      else s.last(i)
    }.toArray
  }

  def keccakSqueeze(s:Array[Bits],nblocks:Int,r:Int):ArrayBuffer[Bits]={
    var n=nblocks
    val out = ArrayBuffer[Bits]()
    val ss = ArrayBuffer[Array[Bits]](s)
    while(n >0){
      val sf=keccakF(ss.last)
      ss += sf
      (0 until r/8).foreach{i=>
        val a: Array[Bits] = sf(i).subdivideIn(8 bits).toArray
        (0 until 8).foreach(ii=> out += a(ii))
      }
      n = n-1
    }
    out
  }


  //SHA3_256
  def sha256(A:Array[Bits],inlen:Int):Array[Bits]={
    val r = 136
    val s: Array[Bits] = keccakAbsorb(r,A,inlen,0x06)
    val t: Array[Bits] = keccakSqueeze(s,1,r).toArray
    t.take(32)
  }

  def sha512(A:Array[Bits],inlen:Int):Array[Bits]={
    val r=72
    val s: Array[Bits] = keccakAbsorb(r,A,inlen,0x06)
    val t: Array[Bits] = keccakSqueeze(s,1,r).toArray
    t.take(64)
  }



  //Shake
  def shakeSqueeze(s:Array[Bits],nblocks:Int,r:Int):(Array[Bits],Array[Bits])={
    var n=nblocks
    val out = ArrayBuffer[Bits]()
    val ss = ArrayBuffer[Array[Bits]](s)
    while(n >0){
      val sf=keccakF(ss.last)
      ss += sf
      (0 until r/8).foreach{i=>
        val a: Array[Bits] = sf(i).subdivideIn(8 bits).toArray
        (0 until 8).foreach(ii=> out += a(ii))
      }
      n = n-1
    }
    (out.toArray,ss.last)
  }

  def shake256(outlen:Int, A:Array[Bits], inlen: Int):Array[Bits]={
    val rate=136
    val nblocks = outlen/rate
    val s = keccakAbsorb(rate,A,inlen,0x1F)
    val (out,s1) = shakeSqueeze(s,nblocks,rate)
    val outBuffer = out.toBuffer
    val lenRest = outlen-nblocks*rate
    if (lenRest>0){
      val (t,_) =shakeSqueeze(s1,1,rate)
      (0 until lenRest).foreach{i=>
        outBuffer += t(i)
      }
    }
    else ()
    outBuffer.toArray
  }

  def shake128(outlen:Int, A:Array[Bits], inlen: Int):Array[Bits]={
    val rate=168
    val nblocks = outlen/rate
    val s = keccakAbsorb(rate,A,inlen,0x1F)
    val (out,s1) = shakeSqueeze(s,nblocks,rate)
    val outBuffer = out.toBuffer
    val lenRest = outlen-nblocks*rate
    if (lenRest>0){
      val (t,_) =shakeSqueeze(s1,1,rate)
      (0 until lenRest).foreach{i=>
        outBuffer += t(i)
      }
    }
    else ()
    outBuffer.toArray
  }

  def shake128Absorb(A:Array[Bits],inlen:Int):Array[Bits]= keccakAbsorb(168,A,inlen,0x1F)

  def shake256Absorb(A:Array[Bits],inlen:Int):Array[Bits]= keccakAbsorb(136,A,inlen,0x1F)

  def shake128Squeeze(s:Array[Bits],nblocks:Int):(Array[Bits],Array[Bits])= shakeSqueeze(s,nblocks,168)

  def shake256Squeeze(s:Array[Bits],nblocks:Int):(Array[Bits],Array[Bits])= shakeSqueeze(s,nblocks,163)


  case class TestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(64 bits), 25)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(64 bits), 25)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.keccakF(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class AbsorbTestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    val mlen = 333
    val p = 6
    val r = 136
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), mlen)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(64 bits), 25)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.keccakAbsorb(r,dataIn.payload.toArray,mlen,p = 6)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class SqueezeTestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    val r = 72
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(64 bits), 25)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 144)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.keccakSqueeze(dataIn.payload.toArray,2,r)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class Sha256TestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 789)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 32)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.sha256(dataIn.payload.toArray,789)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class Sha512TestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 789)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 64)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.sha512(dataIn.payload.toArray,789)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }


  case class Shake256TestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 32)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 289)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.shake256(289,dataIn.payload.toArray,32)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class Shake128TestComponent() extends Component with DSPTestable[Vec[Bits], Vec[Bits]] {
    override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(8 bits), 32)
    override val dataOut: Flow[Vec[Bits]] = master Flow Vec(Bits(8 bits), 289)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(Keccak.shake128(289,dataIn.payload.toArray,32)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }


}

object TestKeccak {
  def main(args: Array[String]): Unit = {
    val testCase =Seq.fill(25)(0).zipWithIndex.map{
      case(i,kk) => BigInt(kk/3*2)
    }
//    val testCase = Seq("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24").map(BigInt(_, 10))
    //    GenRTL(Keccak.TestComponent(), name = "keccakF")
    val GoldenCase = Seq("7794966339828687485","2237506403369326489","5755602821863956646","10784012419077309138","6421531812547137064","3110473539271523026","12529204697441252515","5217802354664122157","16434252210322985707","14654372300625856118","2673853705654832823","12938133096082201953","5685587837960349380","14279659951051099873","12455962307858742184","3540317268215329391","15089817779001084335","11011060210593679472","11989866322646611577","3582975973770822799","686104115152734441","11574130272833881840","12940634756003555852","9627300290483300879","2856143233000619427"
      ).map(BigInt(_, 10))

    val testvalue1 = Seq.fill(333)(0).zipWithIndex.map{
      case(i,kk) => BigInt(kk/3)
    }
    val GoldenValue1= Seq("506097522914230528","1084818905618843912","1663540288323457296","2242261671028070680","6","0","0","0","9223372036854775808","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0").map(BigInt(_,10))
    val GoldenValue2= Seq("9511363526239227300","15831828664161067259","11217333953817089676","15980030623719500662","7340989678675249610","17250275885717883383","16629989244869266332","6166179879057913455","7582536076523394538","3936342834505785256","8096030449996495451","13343775987498125030","11221377676016646061","2842140031502353220","15886567791264463144","8651822530733473837","6730376562701900184","17532917351906547755","1563743313749848668","17030662853973257764","12713302438808185500","5246821002758672754","845112408862782757","13540552002282489717","11438087214124529799"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testKeccakF", Keccak.TestComponent(), Seq(testCase), GoldenCase)
    VivadoSynth(Keccak.TestComponent(),"keccak")
    //doFlowPeekPokeTest("testKeccakAbsorb", Keccak.AbsorbTestComponent(), Seq(testvalue1), GoldenValue2)

    val GoldenValue2res=Seq("73","52","11","3","171","243","198","126","109","167","45","240","52","164","26","96","22","228","229","29","176","218","80","103","254","207","217","188","139","238","130","37","48","197","137","31","180","40","110","244","179","162","235","141","231","18","25","238","69","238","247","192","6","120","12","68","241","90","194","233","138","165","244","4","200","251","107","17","113","103","215","197","153","99","24","136","199","109","159","210","113","189","98","42","73","99","83","221","73","214","218","179","51","94","242","214","111","37","47","216","227","14","158","252","228","78","29","194","0","234","190","217","73","219","14","102","127","8","234","225","42","32","11","165","26","113","142","141","14","180","125","78","147","254","200","18","51","11","175","254","154","82","214","138"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testKeccakSqueeze", Keccak.SqueezeTestComponent(), Seq(GoldenValue2), GoldenValue2res)

    //test SHA3_256
    val testvalue256 = Seq.fill(789)(0).zipWithIndex.map{
      case(i,kk) => BigInt(kk/7)
    }
    val Golden256=Seq(
      "141","223","98","160","246","56","95","68","50","92","138","211","31","133","20","173","101","3","236","121","231","237","139","164","3","205","64","193","81","250","149","63"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testSHA256", Keccak.Sha256TestComponent(), Seq(testvalue256), Golden256)

    val Golden512=Seq(
      "72","13","12","74","219","175","100","33","55","36","221","157","195","228","231","81","199","25","179","117","105","193","218","65","82","16","12","3","188","224","188","17","134","9","25","168","28","86","58","161","139","219","239","73","81","122","241","77","172","122","229","237","202","116","111","164","184","4","39","171","157","152","144","115"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testSHA512", Keccak.Sha512TestComponent(), Seq(testvalue256), Golden512)


    //test shake256 shake128
    val testShake = Seq.fill(32)(0).zipWithIndex.map{
      case(i,kk) => BigInt(kk*17/9)
    }
    val GoldenShake256=Seq(
      "67","248","63","175","77","176","22","148","4","57","19","67","118","234","4","48","86","127","53","17","66","239","187","193","233","78","188","90","193","87","102","213","133","192","103","46","109","136","2","204","15","127","195","83","17","171","201","251","16","90","49","246","147","110","104","153","194","135","9","216","69","220","101","86","120","195","163","225","253","218","182","184","90","68","81","87","231","10","47","59","52","226","251","153","137","198","244","25","148","26","139","52","241","127","236","34","119","174","57","142","248","209","46","118","204","247","144","5","75","151","80","187","208","121","167","219","165","87","148","57","204","186","103","3","253","33","225","157","252","140","57","100","252","174","168","197","100","249","159","178","70","171","25","153","237","172","225","74","103","229","4","167","23","171","103","26","22","140","79","226","209","1","177","55","55","120","27","19","24","130","52","192","68","212","187","105","175","252","0","100","81","36","181","55","199","132","171","46","169","127","227","111","63","84","197","97","246","46","103","205","255","201","223","135","39","61","236","241","94","33","94","150","79","53","230","35","152","165","164","250","202","160","90","175","176","167","217","94","200","55","0","129","57","46","72","136","242","63","84","195","3","38","170","84","174","234","160","88","65","225","149","176","52","22","133","76","168","154","24","230","233","41","207","76","223","214","138","75","71","225","240","6","109","216","247","203","127","148","116","44","130","59","198","79","38","142","106","163","48"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testShake256",Keccak.Shake256TestComponent(),Seq(testShake),GoldenShake256)
    val GoldenShake128=Seq(
      "238","82","49","102","23","57","49","216","52","137","96","82","198","241","30","86","250","164","249","118","194","38","24","213","181","162","128","158","84","238","6","4","11","192","152","1","60","218","214","46","164","114","49","196","102","12","119","127","31","68","30","39","197","162","134","110","162","28","221","47","62","99","12","34","62","225","86","117","39","188","23","66","26","141","4","12","91","141","91","9","97","179","56","69","227","130","126","125","213","67","63","69","128","122","169","252","62","44","158","216","154","111","187","170","23","127","120","183","6","228","195","107","169","36","127","80","210","49","38","86","200","248","230","203","39","33","149","144","32","36","48","49","143","127","128","221","205","105","142","178","128","195","250","166","160","171","161","192","123","179","185","20","65","138","173","223","73","232","152","222","45","86","112","186","16","146","133","172","42","201","202","60","146","137","229","156","185","67","162","118","84","145","152","20","42","10","126","191","38","194","6","21","152","84","251","150","212","175","107","117","202","220","201","203","113","92","132","14","100","3","74","49","195","20","197","3","204","33","71","13","130","198","222","154","39","233","44","88","58","54","105","229","88","218","204","78","201","228","166","174","172","17","140","46","78","13","4","31","13","38","83","118","136","245","120","186","159","227","150","193","219","204","60","138","253","83","244","118","182","222","5","117","179","91","225","154","22","29","29","199","10","170","91","234","145","74","80","187","78"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testShake128",Keccak.Shake128TestComponent(),Seq(testShake),GoldenShake128)

  }
}
