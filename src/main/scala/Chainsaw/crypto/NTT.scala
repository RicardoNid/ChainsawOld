package Chainsaw.crypto

import Chainsaw._
import cc.redberry.rings
import cc.redberry.rings.scaladsl._
import spinal.core._

/** number theoretic transform on Zp
  * @see
  *   Number theoretic transforms to implement fast digital convolution[[https://ieeexplore.ieee.org/document/1451721]]
  * @param ring
  *   Zp
  * @param N
  *   size of the transformation
  */
case class NTT(ring: Ring[Long], N: Int) {

  require(isPow2(N))
  val p: Int = ring.cardinality().intValue() // p of \Z_p

  val omega: Long = 3061 // root of twiddle factor

  /** gs butterfly operator, which is used in DIF butterfly network
    */
  def gsButterfly(u: Long, v: Long, twiddle: Long): (Long, Long) = (ring(u + v), ring((u - v) * twiddle)) // forward

  /** ct butterfly operator, which is used in DIT butterfly network
    */
  def ctButterfly(u: Long, v: Long, twiddle: Long): (Long, Long) = (ring(u + twiddle * v), ring(u - twiddle * v)) // backward

  def getTwiddle(index: Int): Long = ring.pow(omega, index)

  def bitReverse(value: Seq[Long]): Seq[Long] = (0 until N).map(i => value(BigInt(i.toBinaryString.reverse.padTo(log2Up(N), '0'), 2).toInt))

  def NTT(coeffs: Seq[Long], inverse: Boolean = false, fast: Boolean = true): Seq[Long] = {

    val inverseString  = if (inverse) "inverse " else ""
    val fastString     = if (inverse) "fast " else ""
    val inverseN: Long = ring.pow(N, -1)
    //    logger.info(s"start $fastString${inverseString}NTT:\nN:$N, N^-1:$inverseN, omega:$omega")

    val ret = if (fast) {
      def buildStage(dataIns: Seq[Long]) = {
        //        logger.debug(s"input to stage:    ${dataIns.mkString(" ")}")
        val (up, down) = dataIns.splitAt(dataIns.size / 2)
        val step       = N / dataIns.size
        val temp = up.zip(down).zipWithIndex.map { case ((u, v), i) =>
          val twiddle = getTwiddle(if (!inverse) step * i else -step * i)
          if (!inverse) gsButterfly(u, v, twiddle)
          else ctButterfly(u, v, twiddle)
        }
        val ret = temp.map(_._1) ++ temp.map(_._2)
        //        logger.debug(s"output from stage: ${ret.mkString(" ")}")
        ret
      }

      def buildRecursively(dataIns: Seq[Long]): Seq[Long] = {
        if (dataIns.size == 1) dataIns
        else {
          if (!inverse) {
            val temp       = buildStage(dataIns)
            val (up, down) = temp.splitAt(dataIns.size / 2)
            buildRecursively(up) ++ buildRecursively(down)
          } else {
            val (up, down) = dataIns.splitAt(dataIns.size / 2)
            val temp       = buildRecursively(up) ++ buildRecursively(down)
            buildStage(temp)
          }
        }
      }

      if (!inverse) bitReverse(buildRecursively(coeffs))
      else {
        //        logger.info(s"see ${bitReverse(coeffs)}")
        buildRecursively(bitReverse(coeffs)).map(value => ring(value * inverseN))
      }
    } else {
      (0 until N).map { k =>
        if (!inverse) ring(coeffs.zipWithIndex.map { case (value, i) => value * getTwiddle(i * k) }.sum)
        else ring(coeffs.zipWithIndex.map { case (value, i) => value * getTwiddle(-i * k) }.sum * inverseN)
      }
    }

    //    logger.info(s"get NTT \ninput:  ${coeffs.mkString(" ")}\nresult: ${ret.mkString(" ")}")
    ret
  }

  def INTT(coeffs: Seq[Long], fast: Boolean = true): Seq[Long] = NTT(coeffs, inverse = true, fast)
}
