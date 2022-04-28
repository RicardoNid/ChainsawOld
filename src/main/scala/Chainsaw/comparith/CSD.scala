package Chainsaw.comparith

import Chainsaw.ChainsawRand

import scala.annotation.tailrec
import scala.math.pow

object CSD {

  /** Optimal canonic signed digit encoding
    *
    * @return
    *   string of coded bits, MSB -> LSB, 9 stands for -1
    * @see
    *   [[https://www.notion.so/Classical-CSD-Coding-Optimal-CSD-Coding-46f4f962bae14086ac1c9946023f1157 Chainsaw CSD Coding]]
    * @see
    *   ''DSP with FPGA'', algo 2.4
    */
  def optimalCSD(num: Int): String = {
    val raw = num.toBinaryString.reverse + "0" // LSB -> MSB with 0 padded

    val pattern0 = "11+0".r
    val pattern1 = "1101".r
    val pattern2 = "901".r
    val patterns = Array(pattern0, pattern1, pattern2)

    @tailrec
    def process(raw: String, stage: Int = 0): String = {
      val pattern = patterns(stage)
      pattern.findFirstIn(raw) match {
        case None =>
          stage match {
            case 2 => raw // last stage, no match, return
            case _ => process(raw, stage + 1) // no match, next stage
          }
        case Some(x) => // match, replace
          stage match {
            case 0 => process(raw.replace(x, "9" + "0" * (x.length - 2) + "1"), stage)
            case 1 => process(raw.replaceFirst(x, "9011"), stage)
            case 2 => process(raw.replaceFirst(x, "110"), stage)
          }
      }
    }

    process(raw).reverse.dropWhile(_ == '0') // MSB -> LSB, drop the leading 0s
  }

  /** Classic canonic signed digit encoding
    *
    * @return
    *   string of coded bits, MSB -> LSB, 9 stands for -1
    * @see
    *   [[https://www.notion.so/Classical-CSD-Coding-Optimal-CSD-Coding-46f4f962bae14086ac1c9946023f1157 Chainsaw CSD Coding]]
    * @see
    *   DSP with FPGA, algo 2.2
    */
  def classicCSD(num: Int): String = {
    val raw     = num.toBinaryString.reverse + "0" // LSB -> MSB with 0 padded
    val pattern = "11+0".r

    @tailrec
    def process(raw: String): String = {
      pattern.findFirstIn(raw) match {
        case None    => raw
        case Some(x) => process(raw.replaceFirst(x, "9" + "0" * (x.length - 2) + "1"))
      }
    }

    process(raw).reverse // MSB -> LSB
  }

  /** Verify the result of CSD coding by evaluate the value of coded string
    */
  def verifyCSD(coded: String, num: Int): Boolean = {
    coded.reverse.zipWithIndex.map { case (r, i) =>
      r match {
        case '0' => 0
        case '1' => 1 * pow(2, i)
        case '9' => -1 * pow(2, i)
      }
    }.sum == num
  }

  /** Example and test of coding algos
    */
  def main(args: Array[String]): Unit = {

    def testCSD(testCase: Int, encode: Int => String): Unit =
      assert(verifyCSD(encode(testCase), testCase), s"value: $testCase, your coding: ${encode(testCase)}")

    println(s"CSD example: 31 is encoded as ${optimalCSD(31)}")
    (0 until 10000).foreach(_ => testCSD(ChainsawRand.nextInt(10000), optimalCSD))
    (0 until 10000).foreach(_ => testCSD(ChainsawRand.nextInt(10000), classicCSD))
    println("CSD test passed")

  }
}
