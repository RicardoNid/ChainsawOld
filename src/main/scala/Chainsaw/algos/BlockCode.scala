package Chainsaw.algos

import Chainsaw.algos.AlgebraicStructures._
import breeze.linalg._
import spinal.core._

abstract class BlockCode {

  val q, n, k: Int

  implicit val field = FiniteField(q)

  val codeRate = k.toDouble / n.toDouble

  val l = log2Up(q)

  def getHammingDistance(a: DenseVector[FFInt], b: DenseVector[FFInt]) =
    a.toArray.zip(b.toArray).count { case (a, b) => a.value != b.value }

  def encode(u: DenseVector[FFInt]): DenseVector[FFInt]

  def check(r: DenseVector[FFInt]): Boolean

  def decode(r: DenseVector[FFInt]): DenseVector[FFInt]

  def informationSet: Set[DenseVector[FFInt]]

  def codeSet = informationSet.map(encode)

  def minimumHammingDistance: Int

}

abstract class LinearBlockCode() extends BlockCode {

  // k * n generator matrix
  val G: DenseMatrix[FFInt]

  // n-k * k parity check matrix
  //  val H = DenseMatrix.horzcat(-G(::, k until n).t, DenseMatrix.eye[FFInt](n - k))

  override def encode(u: DenseVector[FFInt]) = (u.toDenseMatrix * G).toDenseVector

  //  override def check(r: DenseVector[FFInt]) = (H * r.toDenseMatrix.t).toDenseVector.forall(_.value == 0)

  // todo: implement minimumHammingDistance by calculating  smallest number of linearly dependent columns of the parity-check matrix H
}

abstract class CyclicCode extends Component {

}
