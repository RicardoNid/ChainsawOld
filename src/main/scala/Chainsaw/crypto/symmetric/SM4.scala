package Chainsaw.crypto.symmetric

import Chainsaw.DFG._
import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

case class SM4Config(readAsync: Boolean, usingBRAM: Boolean, usingROM: Boolean, parallelism: Int, comb: Boolean = false) {
  val s0: String = if (readAsync) "A" else "S"
  val s1: String = if (usingBRAM) "B" else "D"
  val s2: String = if (usingROM) "R" else "L"
  val s3: String = parallelism.abs.toString

  override def toString: String = Seq(s0, s1, s2, s3).mkString("_")
}

object SM4 {

  def transformL(data: Bits, forData: Boolean)(implicit config: SM4Config): Bits = {
    require(data.getBitsWidth == 32)
    if (forData) (data ^ data.rotateLeft(2)) ^ (data.rotateLeft(10) ^ data.rotateLeft(18) ^ data.rotateLeft(24))
    else data ^ data.rotateLeft(13) ^ data.rotateLeft(23)
  }

  def FKData: Seq[Bits] = Seq("A3B1BAC6", "56AA3350", "677D9197", "B27022DC")
    .map(BigInt(_, 16)).map(B(_, 32 bits))

  def CKData: Array[Bits] = getBigSeqFromHex(
    "00070E15 1C232A31 383F464D 545B6269" +
      " 70777E85 8C939AA1 A8AFB6BD C4CBD2D9" +
      " E0E7EEF5 FC030A11 181F262D 343B4249" +
      " 50575E65 6C737A81 888F969D A4ABB2B9" +
      " C0C7CED5 DCE3EAF1 F8FF060D 141B2229" +
      " 30373E45 4C535A61 686F767D 848B9299" +
      " A0A7AEB5 BCC3CAD1 D8DFE6ED F4FB0209" +
      " 10171E25 2C333A41 484F565D 646B7279").map(B(_, 32 bits))

  def substitutionS(data: Bits)(implicit config: SM4Config): Bits = {
    require(data.getBitsWidth == 8)

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


    val SROM = Mem(SBoxData)

    if (!config.readAsync && config.usingBRAM) SROM.addAttribute("ram_style", "block")
    else SROM.addAttribute("ram_style", "distributed")

    if (config.readAsync) SROM.readAsync(data.asUInt)
    else SROM.readSync(data.asUInt)
  }

  def transformT(data: Bits, forData: Boolean)(implicit config: SM4Config): Bits = {
    require(data.getBitsWidth == 32)
    data.setName("beforeS", weak = true)
    val afterS = data.subdivideIn(8 bits).reverse
      .map(substitutionS).reduce(_ ## _).asBits
    afterS.setName("afterS", weak = true)
    transformL(afterS, forData)
  }

  def transformF(data: Seq[Bits], key: Bits, forData: Boolean)(implicit config: SM4Config): Bits = {
    require(data.forall(_.getBitsWidth == 32) && key.getBitsWidth == 32)
    val Seq(x0, x1, x2, x3) = data
    val x0Pipelined = if (config.readAsync) x0 else RegNext(x0)
    x0Pipelined ^ transformT((x1 ^ x2) ^ (x3 ^ key), forData)
  }

  val baseLineConfig: SM4Config = SM4Config(
    readAsync = true,
    usingBRAM = false,
    usingROM = true,
    parallelism = 1
  )

  val testString: String = "\t01\t23\t45\t67\t89\tAB\tCD\tEF\tFE\tDC\tBA\t98\t76\t54\t32\t10".replace("\t", "")
  val goldenString: String = "68\t1E\tDF\t34\tD2\t06\t96\t5E\t86\tB3\tE9\t4F\t53\t6E\t42\t46".replace("\t", "")
  val testCase, testKey = BigInt(testString, 16)
  val golden = BigInt(goldenString, 16)

}

object SM4Operators {

  import SM4._

  val xorHardware = new BinaryHardware(Operators.xor, 32 bits, 0, 0)

  def dataTransformHardware(implicit config: SM4Config): DSPHardware[Bits] = DSPHardware(
    impl = (dataIn: Seq[Bits], _: GlobalCount) => Seq(transformF(dataIn.take(4).map(_.resize(32 bits)), dataIn.last.resize(32 bits), forData = true)),
    inDegree = 5,
    outWidths = Seq(32 bits),
    delay = 0 cycles,
    exeTime = 0 sec
  )

  def keyTransformHardware(implicit config: SM4Config): DSPHardware[Bits] = DSPHardware(
    impl = (dataIn: Seq[Bits], _: GlobalCount) => Seq(transformF(dataIn.take(4).map(_.resize(32 bits)), dataIn.last.resize(32 bits), forData = false)),
    5,
    Seq(32 bits),
    0 cycles,
    0 sec
  )

  def main(args: Array[String]): Unit = {

    implicit val config: SM4Config = SM4Config(readAsync = false, usingBRAM = false, usingROM = true, parallelism = 1)

    synthDSPNode(dataTransformHardware.asDSPNode("dataTrans"), Seq.fill(5)(32 bits))
    synthDSPNode(keyTransformHardware.asDSPNode("keyTrans"), Seq.fill(5)(32 bits))
  }
}

case class SM4HardwareSpinal(comb: Boolean) extends Component {

  import SM4._

  implicit val config: SM4Config = baseLineConfig

  val plain: Bits = in Bits (128 bits)
  val key: Bits = in Bits (128 bits)
  val cipher: Bits = out Bits (128 bits)

  val Seq(x0, x1, x2, x3) = plain.subdivideIn(32 bits).reverse
  val Seq(k0, k1, k2, k3) = key.subdivideIn(32 bits).reverse

  // initial calculation on keys
  val Seq(ki0, ki1, ki2, ki3) = Seq(k0, k1, k2, k3).zip(FKData)
    .map { case (key, fk) => key ^ fk }
  // hardware component
  val dataTrans: (Seq[Bits], Bits) => Bits = transformF(_, _, forData = true)
  val keyTrans: (Seq[Bits], Bits) => Bits = transformF(_, _, forData = false)

  // round
  if (comb) {
    val dataAtEachRound: ArrayBuffer[Bits] = ArrayBuffer(x0, x1, x2, x3)
    val keyAtEachRound: ArrayBuffer[Bits] = ArrayBuffer(ki0, ki1, ki2, ki3)

    (0 until 32).foreach { i =>
      val kNext = keyTrans(keyAtEachRound.takeRight(4), CKData(i))
      kNext.setName(s"keyAtRound${i + 1}")
      keyAtEachRound += kNext

      val dataNext = dataTrans(dataAtEachRound.takeRight(4), keyAtEachRound(i + 4))
      dataNext.setName(s"dataAtRound${i + 1}")
      dataAtEachRound += dataNext
    }

    cipher := dataAtEachRound.takeRight(4).reverse.reduce(_ ## _)
  }

  else {
    val xDelayLines = Seq(x0, x1, x2, x3).map(signal => History(RegNext(signal), 5))
    val kDelayLines = Seq(ki0, ki1, ki2, ki3).map(signal => History(signal, 5))

    val dataDelayLineAtEachRound: ArrayBuffer[Vec[Bits]] = ArrayBuffer(xDelayLines: _*)
    val keyDelayLineAtEachRound: ArrayBuffer[Vec[Bits]] = ArrayBuffer(kDelayLines: _ *)

    val lifeTable = Seq.fill(4)(0) ++ (1 to 32)

    def getLifeCycle(round: Int) = lifeTable(round + 4)

    (0 until 32).foreach { i =>

      val sourceGaps = (1 to 4).reverse.map { j =>
        val yourLifeCycle = getLifeCycle(i)
        val previousLifeCycle = getLifeCycle(i - j)
        yourLifeCycle - previousLifeCycle
      }

      val delayLength = (33 - i) min 5

      println(s"from ${sourceGaps.mkString(" ")}, to $delayLength")

      val keySources = keyDelayLineAtEachRound.takeRight(4)
        .zip(sourceGaps).map { case (delayLine, gap) =>
        delayLine(gap)
      }
      val kNext = keyTrans(keySources, CKData(i))
      kNext.setName(s"keyAtRound${i + 1}")

      keyDelayLineAtEachRound += History(kNext, delayLength)

      val dataSources = dataDelayLineAtEachRound.takeRight(4)
        .zip(sourceGaps).map { case (delayLine, gap) =>
        delayLine(gap)
      }
      val dataNext = dataTrans(dataSources, keyDelayLineAtEachRound(i + 4)(1))
      dataNext.setName(s"dataAtRound${i + 1}")

      dataDelayLineAtEachRound += History(dataNext, delayLength)
    }

    cipher := dataDelayLineAtEachRound.takeRight(4).map(_.last).reverse.reduce(_ ## _)
  }
}

object SM4HardwareSpinal {
  def main(args: Array[String]): Unit = {

    val comb = false

    SimConfig.withWave.compile(SM4HardwareSpinal(comb)).doSim { dut =>

      import dut.{clockDomain, plain, key, cipher}

      if (!comb) {
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()
      }
      plain #= SM4.testCase
      key #= SM4.testKey
      if (comb) sleep(1)
      else clockDomain.waitSampling(35)
      println(cipher.toBigInt.toString(16))
      assert(cipher.toBigInt == SM4.golden)
    }
  }
}

case class SM4HardwareDFG(config: SM4Config) extends Component with DSPTestable[Vec[Bits], Bits] {

  import SM4._
  import SM4Operators._
  import config._

  implicit val currentConfig = config
  // data path
  override val dataIn: Flow[Vec[Bits]] = slave Flow Vec(Bits(128 bits), 2)
  override val dataOut: Flow[Bits] = master Flow Bits(128 bits)

  // "main" part
  // naming inputs
  val Seq(data, key) = dataIn.payload
  val Seq(x0, x1, x2, x3) = data.subdivideIn(32 bits).reverse
  Seq(x0, x1, x2, x3).zip(Seq("x0", "x1", "x2", "x3")).foreach { case (signal, name) => signal.setName(name) }
  val Seq(k0, k1, k2, k3) = key.subdivideIn(32 bits).reverse
  Seq(k0, k1, k2, k3).zip(Seq("k0", "k1", "k2", "k3")).foreach { case (signal, name) => signal.setName(name) }

  // declare a DFG
  implicit val sm4CoreDFG: DFGGraph[Bits] = DFGGraph[Bits]("sm4_core")

  // set inputs
  val dataInputs: immutable.Seq[InputNode[Bits]] = (0 until 4).map(i => sm4CoreDFG.addInput(s"dataIn$i"))
  val keyInputs: immutable.Seq[InputNode[Bits]] = (0 until 4).map(i => sm4CoreDFG.addInput(s"keyIn$i"))

  // declare nodes(operators)
  val xorNodes: Seq[DeviceNode[Bits]] = (0 until 4).map(i => xorHardware.asDSPNode(s"xor$i"))
  val dataNodes: Seq[DeviceNode[Bits]] = (0 until 32).map(i => dataTransformHardware.asDSPNode(s"dataTrans_$i"))
  val keyNodes: Seq[DeviceNode[Bits]] = (0 until 32).map(i => keyTransformHardware.asDSPNode(s"keyTrans_$i"))
  sm4CoreDFG.addVertices(dataNodes ++ keyNodes ++ xorNodes: _*) // add them into the DFG

  // set outputs
  val outputNodes: Seq[OutputNode[Bits]] = dataNodes.takeRight(4).map(sm4CoreDFG.setOutput(_))
  sm4CoreDFG.setLatency(0)
  // connecting operators

  // constant drivers
  xorNodes.zip(FKData).foreach { case (xorNode, fk) => xorNode.addConstantDriver(fk, 0) }
  keyNodes.zip(CKData).foreach { case (keyNode, ck) => keyNode.addConstantDriver(ck, 4) }

  // initial transform of keys
  (0 until 4).foreach { i => sm4CoreDFG.addEdge(keyInputs(i), xorNodes(i), 0) }

  // "main" part, data & key round operations
  val allData: Seq[DSPNode[Bits]] = dataInputs ++ dataNodes
  val allKeys: Seq[DeviceNode[Bits]] = xorNodes ++ keyNodes

  // cascading data transformations
  (0 until 32).foreach { i =>
    val sources = allData.slice(i, i + 4)
    val key = allKeys(i + 4)
    val target = allData(i + 4)
    (sources :+ key).foreach(node => sm4CoreDFG.addEdge(node, target, 0))
  }

  // cascading key transformations
  (0 until 32).foreach { i =>
    val sources = allKeys.slice(i, i + 4)
    val target = allKeys(i + 4)
    sources.zipWithIndex.foreach { case (node, i) => sm4CoreDFG.addEdge(node(0), target(i), 0) }
  }

  val retimingValues: Map[DSPNode[Bits], Int] = {
    keyNodes.zipWithIndex.map { case (node, time) => node -> (time + 1) }.toMap ++
      dataNodes.zipWithIndex.map { case (node, time) => node -> (time + 2) }.toMap ++
      outputNodes.map(node => node -> 33).toMap
  }

  val nodeRetimingValues: Map[DSPNode[Bits], Int] = (dataNodes ++ keyNodes).map(node => node -> 1).toMap

  val foldingSet: Seq[Seq[DSPNode[Bits]]] = if (parallelism < 0) dataNodes.grouped(-parallelism).toSeq ++ keyNodes.grouped(-parallelism).toSeq ++ xorNodes.grouped(-parallelism).toSeq
  else null

  // pipelining strategies
  val retimed: DFGGraph[Bits] =
    if (comb) sm4CoreDFG
    else if (readAsync) sm4CoreDFG.retimed(retimingValues)
    else sm4CoreDFG.nodeRetiming(nodeRetimingValues)

  // parallelism strategies
  val parallelized: DFGGraph[Bits] = retimed.parallelized(parallelism, foldingSet)

  // reverse & concatenating
  dataOut.payload := parallelized.impl(Seq(x0, x1, x2, x3) ++ Seq(k0, k1, k2, k3)).reverse.reduce(_ ## _)
  // set latency automatically
  override val latency: Int = parallelized.latency
  printlnGreen(parallelized.latency)
  dataOut.valid := Delay(dataIn.valid, latency, init = False)
}

