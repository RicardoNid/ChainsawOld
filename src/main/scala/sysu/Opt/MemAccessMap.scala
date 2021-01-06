package sysu.Opt

case class MemAccessMap(
                         accessMap: Array[Array[Tuple2[Int, Int]]] // (port, addr)
                       ) {
  // todo : 增加约束,所有数值>=011
  val ports = accessMap.flatten.map(_._1).max + 1
  val depth = accessMap.flatten.map(_._2).max + 1

  def addrSeqs = {
    val result = Array.ofDim[Int](accessMap.size, ports)
    accessMap.zip(result).foreach { case (access, blank) =>
      access.foreach { case (port, addr) =>
        blank(port) = addr
      }
    }
    result
  }
}
