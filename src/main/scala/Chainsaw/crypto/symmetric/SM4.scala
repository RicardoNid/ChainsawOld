package Chainsaw.crypto.symmetric

import Chainsaw.DFG._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class SM4(usingSROM: Boolean = true, usingDFG: Boolean = false) extends Component with DSPTestable[Vec[Bits], Bits] {

  // data
  val FKData: Seq[Bits] = Seq("A3B1BAC6", "56AA3350", "677D9197", "B27022DC")
    .map(BigInt(_, 16)).map(B(_, 32 bits))

  val CKData: Array[Bits] = getBigSeqFromHex(
    "00070E15 1C232A31 383F464D 545B6269 " +
      "70777E85 8C939AA1 A8AFB6BD C4CBD2D9 " +
      "E0E7EEF5 FC030A11 181F262D 343B4249 " +
      "50575E65 6C737A81 888F969D A4ABB2B9 " +
      "C0C7CED5 DCE3EAF1 F8FF060D 141B2229 " +
      "30373E45 4C535A61 686F767D 848B9299 " +
      "A0A7AEB5 BCC3CAD1 D8DFE6ED F4FB0209 " +
      "10171E25 2C333A41 484F565D 646B7279 ").map(B(_, 32 bits))

  val SBoxData: Array[Bits] = getBigSeqFromHex(
    "D6 90 E9 FE CC E1 3D B7 16 B6 14 C2 28 FB 2C 05" +
      " 2B 67 9A 76 2A BE 04 C3 AA 44 13 26 49 86 06 99" +
      " 9C 42 50 F4 91 EF 98 7A 33 54 0B 43 ED CF AC 62" +
      " E4 B3 1C A9 C9 08 E8 95 80 DF 94 FA 75 8F 3F A6" +
      " 47 07 A7 FC F3 73 17 BA 83 59 3C 19 E6 85 4F A8" +
      " 68 6B 81 B2 71 64 DA 8B F8 EB 0F 4B 70 56 9D 35" +
      " 1E 24 0E 5E 63 58 D1 A2 25 22 7C 3B 01 21 78 87 " +
      " D4 00 46 57 9F D3 27 52 4C 36 02 E7 A0 C4 C8 9E " +
      " EA BF 8A D2 40 C7 38 B5 A3 F7 F2 CE F9 61 15 A1" +
      " E0 AE 5D A4 9B 34 1A 55 AD 93 32 30 F5 8C B1 E3" +
      " 1D F6 E2 2E 82 66 CA 60 C0 29 23 AB 0D 53 4E 6F" +
      " D5 DB 37 45 DE FD 8E 2F 03 FF 6A 72 6D 6C 5B 51" +
      " 8D 1B AF 92 BB DD BC 7F 11 D9 5C 41 1F 10 5A D8" +
      " 0A C1 31 88 A5 CD 7B BD 2D 74 D0 12 B8 E5 B4 B0" +
      " 89 69 97 4A 0C 96 77 7E 65 B9 F1 09 C5 6E C6 84" +
      " 18 F0 7D EC 3A DC 4D 20 79 EE 5F 3E D7 CB 39 48").map(B(_, 8 bits))

  def transformL(data: Bits, forData: Boolean): Bits = {
    require(data.getBitsWidth == 32)
    if (forData) (data ^ data.rotateLeft(2)) ^ (data.rotateLeft(10) ^ data.rotateLeft(18) ^ data.rotateLeft(24))
    else data ^ data.rotateLeft(13) ^ data.rotateLeft(23)
  }

  def substitutionS(data: Bits): Bits = {
    require(data.getBitsWidth == 8)
    val SROM = Mem(SBoxData)
    SROM.readAsync(data.asUInt)
  }

  def transformT(data: Bits, forData: Boolean): Bits = {
    require(data.getBitsWidth == 32)
    data.setName("beforeS", weak = true)
    val afterS = data.subdivideIn(8 bits).reverse
      .map(substitutionS).reduce(_ ## _).asBits
    afterS.setName("afterS", weak = true)
    transformL(afterS, forData)
  }

  def transformF(data: Seq[Bits], key: Bits, forData: Boolean): Bits = {
    require(data.forall(_.getBitsWidth == 32) && key.getBitsWidth == 32)
    val Seq(x0, x1, x2, x3) = data
    x0 ^ transformT((x1 ^ x2) ^ (x3 ^ key), forData)
  }

  val dataTrans: (Seq[Bits], Bits) => Bits = transformF(_, _, forData = true)
  val keyTrans: (Seq[Bits], Bits) => Bits = transformF(_, _, forData = false)

  val dataTransformHardware: DSPHardware[Bits] = DSPHardware(
    impl = (dataIn: Seq[Bits], _: GlobalCount) => Seq(dataTrans(dataIn.take(4), dataIn.last)),
    5,
    Seq(32 bits),
    0 cycles,
    0 sec
  )

  val keyTransformHardware: DSPHardware[Bits] = DSPHardware(
    impl = (dataIn: Seq[Bits], _: GlobalCount) => Seq(keyTrans(dataIn.take(4), dataIn.last)),
    5,
    Seq(32 bits),
    0 cycles,
    0 sec
  )

  // data path
  override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(128 bits), 2)
  override val dataOut: Flow[Bits] = master Flow Bits(128 bits)
  override val latency: Int = 33

  // "main" part
  // naming inputs
  val Seq(data, key) = dataIn.payload
  val Seq(x0, x1, x2, x3) = data.subdivideIn(32 bits).reverse
  Seq(x0, x1, x2, x3).zip(Seq("x0", "x1", "x2", "x3")).foreach { case (signal, name) => signal.setName(name) }
  val Seq(k0, k1, k2, k3) = key.subdivideIn(32 bits).reverse
  Seq(k0, k1, k2, k3).zip(Seq("k0", "k1", "k2", "k3")).foreach { case (signal, name) => signal.setName(name) }

  // initial calculation on keys
  val Seq(ki0, ki1, ki2, ki3) = Seq(k0, k1, k2, k3).zip(FKData).map { case (key, fk) => key ^ fk }
  //  val Seq(ki0, ki1, ki2, ki3) = Seq(k0, k1, k2, k3).zip(FKData).map { case (key, fk) => RegNext(key ^ fk) }

  if (usingDFG) {
    val sm4CoreDFG = DFGGraph[Bits]("sm4_core")

    val dataNodes: Seq[DSPNode[Bits]] = (0 until 32).map(i => DeviceNode("dataTrans", dataTransformHardware).copy(s"dataTrans_$i"))
    val keyNodes: Seq[DSPNode[Bits]] = (0 until 32).map(i => DeviceNode("keyTrans", keyTransformHardware).copy(s"keyTrans_$i"))
    val ckNodes: Seq[ConstantNode[Bits]] = CKData.zipWithIndex.map { case (ck, i) => ConstantNode(s"ck_$i", ck) }.toSeq

    sm4CoreDFG.addVertices(dataNodes ++ keyNodes ++ ckNodes: _*)
    val dataInputs = (0 until 4).map(i => sm4CoreDFG.addInput(s"dataIn$i"))
    val keyInputs = (0 until 4).map(i => sm4CoreDFG.addInput(s"keyIn$i"))
    val outputNodes = dataNodes.takeRight(4).map(sm4CoreDFG.setOutput(_))

    // connecting
    val allData = dataInputs ++ dataNodes
    val allKeys = keyInputs ++ keyNodes

    (0 until 32).foreach { i =>
      val sources = allData.slice(i, i + 4)
      val key = allKeys(i + 4)
      val target = allData(i + 4)
      (sources :+ key).foreach(node => sm4CoreDFG.addEdge(node, target, 0))
    }

    (0 until 32).foreach { i =>
      val sources = allKeys.slice(i, i + 4)
      val key = ckNodes(i)
      val target = allKeys(i + 4)
      (sources :+ key).foreach(node => sm4CoreDFG.addEdge(node, target, 0))
    }

    val retimingValues: Map[DSPNode[Bits], Int] =
      dataNodes.zipWithIndex.map{ case (node, time) => node -> (time + 1)}.toMap ++
        keyNodes.zipWithIndex.map{ case (node, time) => node -> time}.toMap ++
        outputNodes.map(node => node -> 32).toMap

    val retimed = sm4CoreDFG.retimed(retimingValues)
    println(retimed)

    dataOut.payload := RegNext(retimed.impl(Seq(x0, x1, x2, x3) ++ Seq(ki0, ki1, ki2, ki3)).reverse.reduce(_ ## _))
  }
  else {

    // hardware component

    val dataRounds = Seq.fill(32)(dataTrans)
    val keyRounds = Seq.fill(32)(keyTrans)

    // round
    val dataAtEachRound = ArrayBuffer[Bits](x0, x1, x2, x3)
    //  val dataAtEachRound = ArrayBuffer[Bits](Seq(x0, x1, x2, x3).map(Delay(_, 2)): _*)
    val keyAtEachRound = ArrayBuffer[Bits](ki0, ki1, ki2, ki3)

    keyRounds.zipWithIndex.foreach { case (keyTrans, i) =>
      val kNext = keyTrans(keyAtEachRound.takeRight(4), CKData(i))
      kNext.setName(s"keyAtRound${i + 1}")
      //    keyAtEachRound += RegNext(kNext)
      keyAtEachRound += kNext
    }

    dataRounds.zipWithIndex.foreach { case (dataTrans, i) =>
      val dataNext = dataTrans(dataAtEachRound.takeRight(4), keyAtEachRound(i + 4))
      dataNext.setName(s"dataAtRound${i + 1}")
      //    dataAtEachRound += RegNext(dataNext)
      dataAtEachRound += dataNext
    }
    dataOut.payload := RegNext(dataAtEachRound.takeRight(4).reverse.reduce(_ ## _))
  }

  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}

object SM4 {
  def main(args: Array[String]): Unit = {
    val testString = "\t01\t23\t45\t67\t89\tAB\tCD\tEF\tFE\tDC\tBA\t98\t76\t54\t32\t10".replace("\t", "")
    val goldenString = "68\t1E\tDF\t34\tD2\t06\t96\t5E\t86\tB3\tE9\t4F\t53\t6E\t42\t46".replace("\t", "")
    val testCase, testKey = BigInt(testString, 16)
    val golden = BigInt(goldenString, 16)


    doFlowPeekPokeTest("testSM4", new SM4(usingDFG = true), Seq(Seq(testCase, testKey)), Seq(golden))



//    VivadoSynth(new SM4(), s"synthSM4")
  }
}
