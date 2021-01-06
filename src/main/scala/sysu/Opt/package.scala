package sysu

package object Opt {
  type matrix = Array[Array[Int]]

//  def transpose2D[T](input: Array[Array[T]]): Array[Array[T]] = {
//    val dim1 = input.size
//    val dim2 = input(0).size
//    input.flatten.zipWithIndex.sortBy { case (ele, i) => i % (dim2) }.map { case (ele, i) => ele }.toArray.grouped(dim1).toArray
//  }
//
//  def transpose3D[T](input: Array[Array[Array[T]]]): Array[Array[Array[T]]] = {
//    val dim1 = input.size
//    val dim2 = input(0).size
//    input.flatten.zipWithIndex.sortBy { case (ele, i) => i % (dim2) }.map { case (ele, i) => ele }.toArray.grouped(dim1).toArray
//  }

}
