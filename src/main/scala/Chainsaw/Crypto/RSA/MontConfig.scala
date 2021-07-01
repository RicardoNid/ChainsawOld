package Chainsaw.Crypto.RSA

import Chainsaw._
import spinal.core._

import scala.math.{ceil, floor}

/**
 * @param lMs sizes of the MontMul that are supported
 * @param w   word size of the MontMulPE
 * @param p   number of the MontMulPE
 */
case class MontConfig(lMs: Seq[Int] = Seq(512, 1024, 2048, 3072, 4096),
                      w: Int = 32,
                      pe: Int = 17,
                      parallel: Boolean = true) {

  val parallelFactor = if (parallel) lMs.max / lMs.min else 1
  val p = if (parallel) floor((lMs.max + 2 + 1).toDouble / w).toInt else pe // when parallel, p = e - 1
  val groupPerInstance = lMs.map(_ / lMs.min)
  val instanceNumber = lMs.map(lMs.max / _)

  val wordPerGroup = lMs.min / w
  val wordPerInstance = lMs.map(_ / w)

  val pPerGroup = p / parallelFactor // PEs number for the smallest lM
  val pPerInstance = lMs.map(lM => p / parallelFactor * (lM / lMs.min)) // actual PEs work for a single instance when in parallel architecture

  val startersAtModes = lMs.indices.map(i => (0 until parallelFactor).filter(_ % groupPerInstance(i) == 0) // instance indices of current mode
    .take(parallelFactor / groupPerInstance(i))
  )

  val ns = lMs.map(_ + 2) // total numbers of iterations, r = 2^(n) > 4M
  val es = ns.map(n => ceil((n + 2).toDouble / w).toInt) // numbers of words
  val rounds = if (parallel) ns.zip(pPerInstance).map { case (n, parallelP) => ceil(n.toDouble / parallelP).toInt }
  else ns.map(n => ceil(n.toDouble / p).toInt) // numbers of rounds to be executed

  if (!parallel) require(p <= es.min, "currently, we would require p <= e as p > e leads to grouped input and thus, more complex input pattern")
  if (parallel) require(lMs.forall(_ % lMs.min == 0), s"for parallel architecture, supported sizes must be multiples of the minimum size")
  require(isPow2(w) && w >= 16)

  // the index of PE that provides the result(starts from 0)
  // FIXME: the parallel situation is only for RSA size
  val outputProviders = if (parallel) (0 until parallelFactor).map(_ * pPerGroup + 1) else ns.map(n => (n - 1) % p)
  // the queue depths needed to connect the data of previous and current round
  // these depths indicate the delays, which will be used in the design
  val queueDepths = es.map(e => if (e - p > 0) e - p else 0)
  // data
  val IIs = es.zip(rounds).map { case (i, i1) => i * i1 } // initiation intervals
  // utilizations, tasks / (PE * round)
  val utilizations = if (parallel) (0 until ns.size).map(i => ns(i).toDouble / (p * rounds(i)) * (lMs.max / lMs(i)))
  else (0 until ns.size).map(i => ns(i).toDouble / (p * rounds(i)))
  val queueUtilizations = if (parallel) lMs.map(lM => (lMs.max / lM) * (lM / lMs.min) / parallelFactor.toDouble)
  else (0 until es.size).map(i => (es(i).toDouble - p) / (es.max - p))
  // n + e - 1 for the whole interval, (round - 1) * (e - p) for waiting - (e - 1) for where the output word started, 1 for start -> run
  val latencies = if (parallel) (0 until ns.size).map(i => (ns(i) + es(i) - 1) + (rounds(i) - 1) * (es(i) - pPerInstance(i)) - es(i) + 1 + 1)
  else (0 until ns.size).map(i => (ns(i) + es(i) - 1) + (rounds(i) - 1) * (es(i) - p) - es(i) + 1 + 1)

  printlnGreen(
    s"\n********systolic array properties report********" +
      s"\n\ta instance of MontMul has been initialized, supported sizes of Montgomery Multiplications are lM = ${lMs.mkString(" ")} " +
      s"\n********AREA/TIMING********" +
      s"\n\tword size w = $w, " +
      s"\n\tnumber of PE p = $p" +
      s"\n\tdepth of queue = ${es.max - p}" +
      //      s"\n\testimated area is , for Xilinx UltraScale, fMax >=" +
      //      s"\n\testimated area is , for TSMC 40nm technology, fMax >=" +
      s"\n********AREA/TIMING********" +
      s"\n********WORKING STATUS********" +
      s"\n\tlMs:                     ${lMs.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tword number e:           ${es.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\trounds r:                ${rounds.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tinitiation intervals II: ${IIs.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tlatenciese:              ${latencies.map(_.toString.padTo(10, ' ')).mkString("")}" +
      s"\n\tPE utilization:          ${utilizations.map(_.toString.padTo(10, ' ').take(8)).mkString("  ")}" +
      s"\n\tqueue utilization:       ${queueUtilizations.map(_.toString.padTo(10, ' ').take(8)).mkString("  ")}" +
      s"\n********WORKING STATUS********" +
      s"\n\tcurrently, we would require p <= e as p > e leads to interleaved input and thus, more complex input pattern" +
      s"\n\tthis systolic accept 1 task as a group, and would finish Montgomery Multiplications in" +
      s"\n\tthe input scheme is as follow: " +
      s"\n\t\tio.run should be set through the calculation, name the first cycle with io.run set as cycle 0" +
      s"\n\t\tfor cycle i <- 0 to e-1, words of M and Y, M(i) and Y(i) should be provided to io.YWordIn and io.MWordIn" +
      s"\n\t\tfor cycle j <- 0 to p-1, bits of X, X(j) should be provided to io.XiIn, then" +
      s"\n\t\tfor cycle j <- e to e + p - 1, continues from X(j - (e - p)), this continues until X is fully consumed" +
      s"\n\tthe output is indicated by valid" +
      s"\n********systolic array properties report********")
}
