package Chainsaw.crypto

import cc.redberry.rings.scaladsl.UnivariateRingZp64
import Chainsaw.ringsUtils._

object FastAlgos {

  def p2pMult(f: Seq[Long], g: Seq[Long])(implicit polyRing: UnivariateRingZp64): Seq[Long] =
    f.zip(g).map { case (f, g) => polyRing.cfRing(f * g) }

  /** accelerate the cyclic convolution on Zp by NTT
    *
    * to learn the principles:
    * @see
    *   Number Theoretic Transforms to Implement Fast Digital Convolution [[https://ieeexplore.ieee.org/document/1451721]]
    */
  def CCByNTT(f: Seq[Long], g: Seq[Long])(implicit polyRing: UnivariateRingZp64): Seq[Long] = {
    val N                            = f.size
    val cfRing                       = polyRing.cfRing
    val ntt: Seq[Long] => Seq[Long]  = NTT(cfRing, N).NTT(_)
    val intt: Seq[Long] => Seq[Long] = NTT(cfRing, N).INTT(_)
    intt(p2pMult(ntt(f), ntt(g)))
  }

  /** accelerate the negative wrapped convolution on Zp by NTT
    *
    * for the concept of NWC
    * @see
    *   Speeding up the Number Theoretic Transform for Faster Ideal Lattice-Based Cryptography [[http://eprint.iacr.org/2016/504.pdf]]
    */
  def NWCByNTT(f: Seq[Long], g: Seq[Long])(implicit polyRing: UnivariateRingZp64): Seq[Long] = {
    require(f.size == g.size) // TODO: remove this condition by padding
    val N            = f.size
    val cfRing       = polyRing.cfRing
    val phi          = cfRing.getNthRoot(2 * N)
    val positivePhis = f.indices.map(i => cfRing.pow(phi, i))
    val negativePhis = f.indices.map(i => cfRing.pow(phi, -i))
    val modifiedF    = p2pMult(f, positivePhis)
    val modifiedG    = p2pMult(g, positivePhis)
    val modifiedRet  = CCByNTT(modifiedF, modifiedG)
    p2pMult(modifiedRet, negativePhis)
  }
}
