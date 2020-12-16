import breeze.numerics.ceil

case class DNNModel( // 高度简化的参数
                     N: Array[Int] = Array(2, 2, 3),
                     C: Array[Int] = Array(2, 3, 3),
                     H: Array[Int] = Array(2, 3, 3),
                     K: Array[Int] = Array(2, 3, 3),
                     dspCount: Int = 120,
                     dspFactor: Int = 1,
                     W_h: Array[Int] = Array(1, 1, 1),
                     W_w: Array[Int] = Array(1, 1, 1),
                     I_w: Array[Int] = Array(1, 1, 1)
                   ) {
  require(C.length == N.length && H.length == N.length && K.length == N.length)
  val layerCount = N.length

  def div(a: Int, b: Int) = ceil(a.toDouble / b.toDouble)

  def ops(i: Int) = N(i) * C(i) * K(i) * K(i) * H(i) * H(i)

  def opsAll = (0 until layerCount).map(ops(_)).reduce(_ + _)

  def cycles(i: Int, abc: Tuple3[Int, Int, Int]) // formula : 3-8
  = div(N(i), abc._1) * div(C(i) * K(i) * K(i), abc._2) * div(H(i) * H(i), abc._3)

  def cycleBound = opsAll / (dspCount * dspFactor)

  //
  //  def getABC
  //
  //  def abcList =

}

println(DNNModel().opsAll)
println(DNNModel().cycleBound)
println(DNNModel().ops(1) / DNNModel().cycleBound)



