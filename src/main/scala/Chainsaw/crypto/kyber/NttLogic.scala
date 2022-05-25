package Chainsaw.crypto.kyber

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.crypto.kyber.Reduce.{barrettReduce, csubq, montgomeryReduce}
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


object NttLogic {

  val zetas = Array(2285, 2571, 2970, 1812, 1493, 1422, 287, 202, 3158, 622, 1577, 182, 962,
    2127, 1855, 1468, 573, 2004, 264, 383, 2500, 1458, 1727, 3199, 2648, 1017,
    732, 608, 1787, 411, 3124, 1758, 1223, 652, 2777, 1015, 2036, 1491, 3047,
    1785, 516, 3321, 3009, 2663, 1711, 2167, 126, 1469, 2476, 3239, 3058, 830,
    107, 1908, 3082, 2378, 2931, 961, 1821, 2604, 448, 2264, 677, 2054, 2226,
    430, 555, 843, 2078, 871, 1550, 105, 422, 587, 177, 3094, 3038, 2869, 1574,
    1653, 3083, 778, 1159, 3182, 2552, 1483, 2727, 1119, 1739, 644, 2457, 349,
    418, 329, 3173, 3254, 817, 1097, 603, 610, 1322, 2044, 1864, 384, 2114, 3193,
    1218, 1994, 2455, 220, 2142, 1670, 2144, 1799, 2051, 794, 1819, 2475, 2459,
    478, 3221, 3021, 996, 991, 958, 1869, 1522, 1628)

  val zetas_inv = Array(1701, 1807, 1460, 2371, 2338, 2333, 308, 108, 2851, 870, 854, 1510, 2535,
    1278, 1530, 1185, 1659, 1187, 3109, 874, 1335, 2111, 136, 1215, 2945, 1465,
    1285, 2007, 2719, 2726, 2232, 2512, 75, 156, 3000, 2911, 2980, 872, 2685,
    1590, 2210, 602, 1846, 777, 147, 2170, 2551, 246, 1676, 1755, 460, 291, 235,
    3152, 2742, 2907, 3224, 1779, 2458, 1251, 2486, 2774, 2899, 1103, 1275, 2652,
    1065, 2881, 725, 1508, 2368, 398, 951, 247, 1421, 3222, 2499, 271, 90, 853,
    1860, 3203, 1162, 1618, 666, 320, 8, 2813, 1544, 282, 1838, 1293, 2314, 552,
    2677, 2106, 1571, 205, 2918, 1542, 2721, 2597, 2312, 681, 130, 1602, 1871,
    829, 2946, 3065, 1325, 2756, 1861, 1474, 1202, 2367, 3147, 1752, 2707, 171,
    3127, 3042, 1907, 1836, 1517, 359, 758, 1441)


  def fqmul(a: SInt, b: SInt): SInt = {
    montgomeryReduce(a * b)
  }


  def ntt(r: Array[SInt]): Array[SInt] = {
    require(r.length==256 && (r.forall(_.getWidth==16)))
    var k = 1
    var len = 128
    var rnRound = ArrayBuffer[Array[SInt]](r)
    var ret = r
    while (len >= 2) {
      var i = 0
      while (i < 256) {
        var j = i
        while (j < (i + len)) {
          val t = fqmul(zetas(k), ret(j + len));
          ret(j + len) = ret(j) - t
          ret(j) = ret(j) + t
          j = j + 1
        }
        i = j + len
        k = k + 1
      }
      len = len >> 1
    }
    ret
  }

  //
  def invntt(r: Array[SInt]): Array[SInt] = {
    require(r.length==256 && r.forall(_.getWidth==16))
    var k = 0
    var len = 2
    var ret = r
    while (len <= 128) {
      var i = 0
      while (i < 256) {
        var j = i
        while (j < i + len) {
          val t = ret(j)
          ret(j) = barrettReduce(t + ret(j + len))
          ret(j + len) = t - ret(j + len)
          ret(j + len) = fqmul(zetas_inv(k), ret(j + len))
          j = j + 1
        }
        k = k + 1
        i = j + len
      }
      len = len << 1
    }

    for(i<- 0 until 256){
      ret(i)=fqmul(ret(i),zetas_inv(127))
    }
    ret
  }

   def polyvecntt(r: Array[SInt]): Array[SInt] ={
     val a=ntt(r)
     polyReduce(a)
   }

  //Multiplication of polynomials in Zq[X]/(X^2-zeta) used for multiplication of elements in Rq in NTT domain
  def basemul(a: Array[SInt],b: Array[SInt],zeta:SInt):Array[SInt]={
    val r0 = fqmul(fqmul(a(1),b(1)),zeta)
    val rr0 = r0+fqmul(a(0),b(0))

    val r1 =fqmul(a(0),b(1))
    val rr1 = r1+ fqmul(a(1),b(0))
    Array(rr0,rr1)
  }

  def polyPointBasemul(A:Array[SInt], B:Array[SInt]):Array[SInt]={
    val ret =ArrayBuffer[Array[SInt]]()
    (0 until 64).map{i=>
      val v1=basemul(A.slice(4*i,4*i+2),B.slice(4*i,4*i+2),zetas(64+i))
      val v2 =basemul(A.slice(4*i+2,4*i+4),B.slice(4*i+2,4*i+4),-zetas(64+i))
      ret += (v1,v2)
    }
    ret.flatten.toArray
  }
  def polyMont(r:Array[SInt]):Array[SInt]={
    val f: int32 =1353
    r.map(i => montgomeryReduce(i*f))
  }

  def polyCsubq(r:Array[SInt]):Array[SInt]= r.map(i=>csubq(i))

  def polyReduce(r:Array[SInt]):Array[SInt]= r.map(i=>barrettReduce(i))

  def polyAdd(A:Array[SInt],B:Array[SInt]):Array[SInt] = A.zip(B).map{case(a,b)=> a+b }

  def polySub(A:Array[SInt],B:Array[SInt]):Array[SInt] = A.zip(B).map{case(a,b)=> a-b }

  def polyvecPoint(A:Array[SInt], B:Array[SInt]):Array[SInt]={
    val ret: Array[Array[SInt]] =A.grouped(256).zip(B.grouped(256)).map{case(a,b) =>
      polyPointBasemul(a,b)
    }.toArray
    val ret1: Array[SInt] = ret.reduce(polyAdd(_,_))
    polyReduce(ret1)
  }

  case class polyMontComponent() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]] = slave Flow Vec(SInt(16 bits), 256)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(polyMont(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }



  case class nttComponent() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]] = slave Flow Vec(SInt(16 bits), 256)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(ntt(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class invnttComponent() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]] = slave Flow Vec(SInt(16 bits), 256)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 256)
    override val latency: int32 = 1

    dataOut.payload := RegNext(Vec(invntt(dataIn.payload.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

  case class polyBasmulComponent() extends Component with DSPTestable[Vec[SInt], Vec[SInt]] {
    override val dataIn: Flow[Vec[SInt]] = slave Flow Vec(SInt(16 bits), 256)
    override val dataOut: Flow[Vec[SInt]] = master Flow Vec(SInt(16 bits), 256)
    override val latency: int32 = 1
    val testvalue2 = Seq.fill(256)(0).zipWithIndex.map {
      case (i, kk) => B(kk/3+8,8 bits).asSInt
    }

    dataOut.payload := RegNext(Vec(polyPointBasemul(dataIn.payload.toArray,testvalue2.toArray)))
    dataOut.valid := Delay(dataIn.valid, latency, init = False)
  }

}

object TestNtt {
  def main(args: Array[String]): Unit = {
    val testvalue = Seq.fill(256)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk*128)
    }
    val Goldenvalue = Seq("4644","-2030","7794","-4768","9019","460","9963","-2690","4898","-1749","2584","-1383","6938","-404","4040","1196","2613","2914","1501","2936","551","1897","1111","1001","5878","-505","3532","-1641","4693","-1941","1505","-61","1682","3195","-1036","1481","2332","2751","4654","45","2888","923","402","1459","5185","-3196","3069","-202","2115","1515","3943","3829","-404","2653","1982","-461","2739","214","1705","3374","3629","6198","1307","3246","805","-4617","-627","-5003","-57","-2450","-1709","-1330","-1193","1195","725","601","-2064","-3926","-2992","-470","-309","-4208","-1895","-1304","1205","-332","-625","-3304","-110","811","-2032","4485","823","1279","-1113","-1747","1400","1130","-2164","3276","-92","19","2624","3159","-1577","-1107","-405","-891","-3938","2692","-1496","-278","631","-97","-2593","-1613","-517","-2022","-517","-1824","900","-2218","476","-4894","2076","-2585","3304","-875","668","-1555","2988","1247","-203","-1235","2955","-329","653","2216","-1923","-162","-991","-1715","53","1037","-900","1467","-3950","-337","316","1058","754","540","-1698","-1977","-2398","-2253","-3804","999","-3240","615","-5781","1767","-3249","3209","-3077","649","-49","2615","-4426","-1491","-1996","-203","-579","2415","-1387","-561","9","-5586","-1961","-3172","1986","-963","2822","-2639","615","-2837","-507","-1187","-2766","1866","122","-858","-2907","2068","-2073","2272","-3298","-1382","-538","-1086","-2313","386","-1203","2654","-4014","-315","-1270","2915","1015","-2116","-335","-5014","2266","-1864","-602","-1710","-4417","448","-2255","-2418","-84","-2345","-4","-1261","-4259","892","-1973","1588","-4302","1127","-5122","1517","-1212","5812","-1660","3908","-3520","1959","-5056","1557","-4127","2302","-1575","2694","-4389","3163","-1769","1805","-495","2427","2461","1289","-2657","3503","-1769","5161").map(BigInt(_, 10))
    val GoldenvalueInv = Seq("472","0","-1418","-1418","-608","-608","-438","-438","-1481","-1481","417","417","916","916","-353","-353","-1612","-1612","-1382","-1382","-1190","-1190","-1192","-1192","899","899","-658","-658","114","114","-1525","-1525","-1548","-1548","-1362","-1362","1568","1568","1216","1216","1386","1386","1459","1459","-1063","-1063","274","274","-1071","-1071","843","843","-1392","-1392","-862","-862","-764","-764","1031","1031","-1016","-1016","692","692","-382","-382","-612","-612","-666","-666","-1506","-1506","1588","1588","1150","1150","115","115","-334","-334","115","115","721","721","1001","1001","-1200","-1200","-377","-377","109","109","-489","-489","-224","-224","510","510","-326","-326","-1640","-1640","-703","-703","-808","-808","1117","1117","1299","1299","1019","1019","-203","-203","-123","-123","1618","1618","-1110","-1110","866","866","1190","1190","1156","1156","-1142","-1142","951","951","-1142","-1142","1156","1156","1190","1190","866","866","-1110","-1110","1618","1618","-123","-123","-203","-203","1019","1019","1299","1299","1117","1117","-808","-808","-703","-703","-1640","-1640","-326","-326","510","510","-224","-224","-489","-489","109","109","-377","-377","-1200","-1200","1001","1001","721","721","115","115","-334","-334","115","115","1150","1150","1588","1588","-1506","-1506","-666","-666","-612","-612","-382","-382","692","692","-1016","-1016","1031","1031","-764","-764","-862","-862","-1392","-1392","843","843","-1071","-1071","274","274","-1063","-1063","1459","1459","1386","1386","1216","1216","1568","1568","-1362","-1362","-1548","-1548","-1525","-1525","114","114","-658","-658","899","899","-1192","-1192","-1190","-1190","-1382","-1382","-1612","-1612","-353","-353","916","916","417","417","-1481","-1481","-438","-438","-608","-608","-1418","-1418").map(BigInt(_, 10))
    doFlowPeekPokeTest("testntt",NttLogic.nttComponent(),Seq(testvalue),Goldenvalue)
    doFlowPeekPokeTest("testinvntt", NttLogic.invnttComponent(), Seq(testvalue), GoldenvalueInv)

    val testvalue2 = Seq.fill(256)(0).zipWithIndex.map {
      case (i, kk) => BigInt(kk/2)
    }
    val GoldenPolyBasemul =Seq(
      "0","0","2127","2873","-392","-574","-1413","-3176","932","880","-65","-1384","-2823","-2296","2211","-386","296","1862","1001","2640","-1067","-925","822","2506","153","-1684","-549","-737","-1265","548","686","-1952","-1584","178","1456","2646","1148","1836","-673","1820","-402","2142","801","3022","-732","-2469","-1362","-964","1993","1606","1753","627","1546","-14","-1019","-2412","-258","1121","596","-1666","415","-2374","808","-987","-405","738","1040","1720","-1891","-2368","586","540","3235","3212","528","307","-1997","-2260","1697","2102","-820","380","-2099","-1004","-1495","-1610","-719","-2149","-86","-2350","-1282","-1266","349","-622","-775","360","1562","3134","-207","-1697","1008","468","199","-1726","-1714","-2045","-2450","-2026","-2682","-2530","-1369","-1666","-509","-464","-1261","722","1478","2769","-1040","-1504","1439","1372","-1863","-2056","-682","1512","-824","-580","1945","504","253","1926","784","1524","970","462","-658","-262","838","1026","-182","1147","1776","1606","-419","-2074","-1529","-770","-86","872","-480","-1118","-2157","-1960","331","-2464","-1065","-2764","-1209","-2423","-2346","-1744","-1779","-354","400","1170","617","3032","-1619","-546","764","2161","-1023","-1452","829","3318","-712","550","-1977","-1880","-1343","-2078","-149","-334","2295","1748","1500","3240","107","-491","2158","2774","829","-702","-1970","79","2223","1198","1170","-588","-364","-1953","-240","-2980","-1580","-3076","102","71","13","-3102","-1143","-1508","-672","-507","1517","832","-2885","-2542","-1103","-358","982","2164","656","480","2247","518","-288","894","-106","900","1390","2121","-2173","-2978","-1176","-1282","2110","1122","-2012","-2794","-996","592","-1172","-2479","2155","1446","-228","-136","143","-2024","1762","3084","523","3192","-559","-842"
    ).map(BigInt(_,10))
    //doFlowPeekPokeTest("testBasmul", NttLogic.polyBasmulComponent(), Seq(testvalue2), GoldenPolyBasemul)

  }



}