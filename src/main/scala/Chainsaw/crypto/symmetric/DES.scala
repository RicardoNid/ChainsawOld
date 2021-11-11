package Chainsaw.crypto.symmetric

import Chainsaw._
import Chainsaw.dspTest._
import spinal.core._
import spinal.lib._

import javax.crypto.spec.IvParameterSpec
import javax.crypto.{Cipher, KeyGenerator, SecretKey}
import scala.collection.mutable.ArrayBuffer



/** this is the DES reference model
 *
 */

object DESRef {
  val initVector: Array[Byte] = Seq(22, 33, 11, 44, 55, 99, 66, 77).map(_.toByte).toArray
  val aps = new IvParameterSpec(initVector)

  val secretKeyCBC: SecretKey = KeyGenerator.getInstance("DES").generateKey()
  // setting encryption mode
  val encryptCBC: Cipher = Cipher.getInstance("DES/CBC/PKCS5Padding")
  encryptCBC.init(Cipher.ENCRYPT_MODE, secretKeyCBC, aps)
  // setting decryption mode
  val decryptCBC: Cipher = Cipher.getInstance("DES/CBC/PKCS5Padding")
  decryptCBC.init(Cipher.DECRYPT_MODE, secretKeyCBC, aps)

  // constructing cipher instances

  // this mode can be used as the test bench of DES core
  // setting encryption mode
  val secretKeyECB: SecretKey = (0 until 10).map(_ => KeyGenerator.getInstance("DES").generateKey())
    .filter(key => BigInt(key.getEncoded) > 0).head // get a positive one
  val encrypt: Cipher = Cipher.getInstance("DES/ECB/PKCS5Padding")
  encrypt.init(Cipher.ENCRYPT_MODE, secretKeyECB)
  // setting decryption mode
  val decrypt: Cipher = Cipher.getInstance("DES/ECB/PKCS5Padding")
  decrypt.init(Cipher.DECRYPT_MODE, secretKeyECB)

  def genTestCase: Array[Byte] = DSPRand.nextBigInt(63).toByteArray

  def testDESEncrypt(testCase: Array[Byte], yours: Array[Byte]): Unit = {
    assert(yours.sameElements(encrypt.doFinal(testCase)))
  }

  def testDESDecrypt(testCase: Array[Byte], yours: Array[Byte]): Unit = {
    assert(yours.sameElements(decrypt.doFinal(testCase)))
  }
}

object DES {

  import DESRef._

  /**
   * @param dataIn cipher
   */
  def des(dataIn: Bits, key: Bits): Bits = {

    require(dataIn.getBitsWidth == 64 && key.getBitsWidth == 64)



    def constructSBox(string: String) = getSeqFromText(string).grouped(17).toSeq.map(_.drop(1)).flatten

    // permutation data
    val IPData: Seq[Int] = getSeqFromText("58 50 42 34 26 18 10 2 60 52 44 36 28 20 12 4 62 54 46 38 30 22 14 6 64 56 48 40 32 24 16 8 57 49 41 33 25 17 9 1 59 51 43 35 27 19 11 3 61 53 45 37 29 21 13 5 63 55 47 39 31 23 15 7")
    val EData = getSeqFromText("32 1 2 3 4 5 4 5 6 7 8 9 8 9 10 11 12 13 12 13 14 15 16 17 16 17 18 19 20 21 20 21 22 23 24 25 24 25 26 27 28 29 28 29 30 31 32 1")
    val PData = getSeqFromText("16 7 20 21 29 12 28 17 1 15 23 26 5 18 31 10 2 8 24 14 32 27 3 9 19 13 30 6 22 11 4 25")
    val PC1Data = getSeqFromText("57 49 41 33 25 17 9 1 58 50 42 34 26 18 10 2 59 51 43 35 27 19 11 3 60 52 44 36 63 55 47 39 31 23 15 7 62 54 46 38 30 22 14 6 61 53 45 37 29 21 13 5 28 20 12 4")
    val PC2Data = getSeqFromText("14 17 11 24 1 5 3 28 15 6 21 10 23 19 12 4 26 8 16 7 27 20 13 2 41 52 31 37 47 55 30 40 51 45 33 48 44 49 39 56 34 53 46 42 50 36 29 32")

    val initPermutation: Seq[Bool] => Seq[Bool] = permutation[Bool](_, IPData) // initial permutation
    val lastPermutation: Seq[Bool] => Seq[Bool] = inversePermutation[Bool](_, IPData) // last permutation, the inverse of IP
    val eBox: Seq[Bool] => Seq[Bool] = permutation[Bool](_, EData) // E Box
    val pBox: Seq[Bool] => Seq[Bool] = permutation[Bool](_, PData) // P Box
    val pc1Box: Seq[Bool] => Seq[Bool] = permutation[Bool](_, PC1Data)
    val pc2Box: Seq[Bool] => Seq[Bool] = permutation[Bool](_, PC2Data)

    val S1Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S2Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S3Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S4Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S5Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S6Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S7Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")
    val S8Data = constructSBox("0 14 4 13 1 2 15 11 8 3 10 6 12 5 9 0 7 1 0 15 7 4 14 2 13 1 10 6 12 11 9 5 3 8 2 4 1 14 8 13 6 2 11 15 12 9 7 3 10 5 0 3 15 12 8 2 4 9 1 7 5 11 3 14 10 0 6 13")

    val shiftSizes = Seq(1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1)

    def sBox(data: Bits, table: Seq[Int]) = {
      require(data.getBitsWidth == 6)
      val SROM = Mem(table.map(U(_, 4 bits)))
      val address = (data.msb ## data(4 downto 1) ## data.lsb).asUInt
      SROM.readAsync(address)
    }

    // S Boxes
    val sBoxes: Seq[Bits => UInt] = Seq(S1Data, S2Data, S3Data, S4Data, S5Data, S6Data, S7Data, S8Data)
      .map { data =>
        val box: Bits => UInt = sBox(_, data)
        box
      }

    def fOperation(data: Bits, key: Bits) = {
      require(data.getBitsWidth == 32 && key.getBitsWidth == 48, s"${data.getBitsWidth}, ${key.getBitsWidth}")
      val afterE: Bits = eBox(data.asBools).asBits()
      val afterXOR: Bits = afterE ^ key

      val groupsForS: Seq[Seq[Bool]] = afterXOR.asBools.grouped(6).toSeq
      val afterS: UInt = groupsForS.zip(sBoxes).map { case (bools, sBox) => sBox(bools.asBits()) }
        .reduce(_ @@ _)

      pBox(afterS.asBools).asBits()
    }

    def round(data: Bits, key: Bits, last: Boolean = false): Bits = {
      require(data.getBitsWidth == 64)
      val (left, right) = data.splitAt(32)
      val newLeft = right
      val newRight = fOperation(right, key) ^ left
      if (last) newRight ## newLeft else newLeft ## newRight
    }

    // the "shift" should not really happen
    def keyShift(key: Bits, shiftSize: Int) = {
      require(key.getBitsWidth == 56)
      val (left, right) = key.splitAt(28)
      val shiftedLeft = left.rotateLeft(shiftSize)
      val shiftedRight = right.rotateLeft(shiftSize)

      val nextKey = shiftedLeft ## shiftedRight
      val currentKey = pc2Box(nextKey.asBools).asBits()

      (nextKey, currentKey)
    }

    // "main" part

    // hardware components
    val notLastRound: (Bits, Bits) => Bits = round(_, _, true)
    val lastRound: (Bits, Bits) => Bits = round(_, _, false)
    val rounds: Seq[(Bits, Bits) => Bits] = Seq.fill(15)(notLastRound) :+ lastRound

    val keyShifts: Seq[Bits => (Bits, Bits)] = shiftSizes.map { size =>
      val keyshift: Bits => (Bits, Bits) = keyShift(_, size)
      keyshift
    }

    // initial permutation
    val dataAfterIP = initPermutation(dataIn.asBools).asBits()
    val keyAfterIP = pc1Box(key.asBools).asBits()

    val keysEachRoundForNext = ArrayBuffer[Bits](keyAfterIP)
    val keysEachRoundForEncrypt = ArrayBuffer[Bits]()

    keyShifts.zipWithIndex.foreach { case (keyShift, i) =>
      val lastKey = keysEachRoundForNext.last
      val (nextKey, currentKey) = keyShift(lastKey)
      nextKey.setName(s"keyAtRound${i + 1}")
      keysEachRoundForNext += nextKey
      keysEachRoundForEncrypt += currentKey
    }

    val dataEachRound = ArrayBuffer[Bits](dataAfterIP)
    rounds.zip(keysEachRoundForEncrypt).zipWithIndex.foreach { case ((roundFunction, key), i) =>
      val data = roundFunction(dataEachRound.last, key)
      data.setName(s"dataAtRound${i + 1}")
      dataEachRound += roundFunction(dataEachRound.last, key)
    }

    lastPermutation(dataEachRound.last.asBools).asBits()
  }

  def main(args: Array[String]): Unit = {
    //    println(encrypt.getIV.mkString(" "))
    // byte array is the "native" form of Java String

    val plain = "my name is Li Tianrui!".getBytes
    val encrypted: Array[Byte] = encrypt.doFinal(plain)
    println(new String(encrypted))
    println(encrypted.size)
    val decrypted: Array[Byte] = decrypt.doFinal(encrypted)
    println(decrypted.size)
    println(new String(decrypted))
    println()
    println(secretKeyCBC.getEncoded.mkString(" "))

    val testCases = (0 until 16).map(_ => genTestCase)
    val golden = testCases.map(encrypt.doFinal(_))

    printlnGreen(testCases.head.size)
    printlnGreen(golden.head.size)
    printlnGreen(golden.map(BigInt(_).toString(16)).mkString(" "))
    printlnGreen(testCases.map(BigInt(_).toString(16)).mkString(" "))

    doFlowPeekPokeTest(
      name = "testDES",
      dut = new Component with DSPTestable[Bits, Bits] {
        override val dataIn: Flow[Bits] = slave Flow Bits(64 bits)
        val key = B(BigInt(secretKeyECB.getEncoded), 64 bits)
        override val dataOut: Flow[Bits] = master Flow Bits(64 bits)
        override val latency: Int = 1

        dataOut.payload := RegNext(des(dataIn.payload, key))
        dataOut.valid := Delay(dataIn.valid, latency, init = False)
      },
      testCases = testCases.map(BigInt(_)),
      golden = golden.map(BigInt(_))
    )
  }
}
