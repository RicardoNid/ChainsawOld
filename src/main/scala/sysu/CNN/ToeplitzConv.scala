package sysu.CNN

import spinal.core._
import sysu.Opt.Index
import sysu.Opt.functions.{getGroups, getMap, validVirtualAddr}
import sysu.util._

trait PadMode

case object BR extends PadMode

case class ToeplitzConv(Nof: Int = 1, Nif: Int = 2, Niy: Int = 224, Nix: Int = 224, Nky: Int = 7, Nkx: Int = 7,
                        Stride: Int = 2,
                        Wh: Int = 16, Ww: Int = 1, Iw: Int = 3) {

  require(Nix == Niy && Nkx == Nky)
  require(Nkx % 2 == 1)

  // Nox = Nix / Stride
  require(Nix % Stride == 0)
  //  require((Nkx - Stride) % 2 == 0)
  val Pad = (Nkx - Stride + 1) / 2 // fixme 仔细思考非准确padding带来的影响
  val Nox = Nix / Stride
  val Noy = Niy / Stride
  val Ih = Ww

  val memGrid = Array(1000, 10000)
  val memeIndex = (port: Int, addr: Int) => Index(memGrid, port, addr)

  val inputGrid = Array(Nif, Nix, Niy)
  val inputSize = inputGrid.product
  val inputIndex = (c: Int, h: Int, w: Int) => {
    if ((0 until Nif).contains(c) && (Pad until Pad + Niy).contains(h) && (Pad until Pad + Nix).contains(w)) Index(inputGrid, c, h - Pad, w - Pad)
    else Index(inputGrid, 0, 0, -1)
    //    Index(inputGrid, c, h, w) // 对于padding行列,使用真实的全0存储
  }

  val weightGrid = Array(Nof, Nif, Nix, Niy)
  val weightSize = weightGrid.product
  val weightIndex = (n: Int, c: Int, h: Int, w: Int) => Index(weightGrid, n, c, h, w)

  val outputGrid = Array(Nof, Nox, Noy)
  val outputSize = outputGrid.product
  val outputIndex = (n: Int, h: Int, w: Int) => Index(outputGrid, n, h, w)

  val inputData = Array.tabulate(Nif, Niy + Pad * 2, Nix + Pad * 2)(inputIndex(_, _, _)) // padded
  val weightData = Array.tabulate(Nof, Nif, Nky, Nkx)(weightIndex(_, _, _, _))
  val outputData = Array.tabulate(Nof, Nox, Noy)(outputIndex(_, _, _))

  def weightMatrix = weightData.map(_.map(_.flatten).flatten)

  def outputMatrix(rowId: Int) = outputData.map(_.flatten).map(_.slice(rowId * Nox, (rowId + 1) * Nox))

  def transpose2D(input: Array[Array[Index]]): Array[Array[Index]] = {
    val dim1 = input.size
    val dim2 = input(0).size
    input.flatten.zipWithIndex.sortBy { case (ele, i) => i % (dim2) }.map { case (ele, i) => ele }.toArray.grouped(dim1).toArray
  }

  def transpose3D(input: Array[Array[Array[Index]]]): Array[Array[Array[Index]]] = {
    val dim1 = input.size
    val dim2 = input(0).size
    input.flatten.zipWithIndex.sortBy { case (ele, i) => i % (dim2) }.map { case (ele, i) => ele }.toArray.grouped(dim1).toArray
  }

  // todo : 支持非正方形
  def toeplitz2D(input: Array[Array[Index]]) = {
    val slided =
      for (oy <- 0 until Nox; ox <- 0 until Noy)
        yield input.slice(oy * Stride, oy * Stride + Nkx).map(_.slice(ox * Stride, ox * Stride + Nkx)).flatten
    transpose2D(slided.toArray)
  }

  def pad2D(input: Array[Array[Index]], h: Int, w: Int, mode: PadMode) = {
    val padded = Array.ofDim[Index](h, w)
    val hIn = input.size
    val wIn = input(0).size
    for (i <- 0 until h; j <- 0 until w) padded(i)(j) = if (i < hIn && j < wIn) input(i)(j) else inputIndex(0, 0, -1)
    padded
  }

  def inputToeplitzed(rowId: Int) = inputData.map(toeplitz2D).reduce(_ ++ _).map(_.slice(rowId * Nox, (rowId + 1) * Nox))

  def inputToeplitzedPadded(rowId: Int) = {
    val toeplitzed = inputToeplitzed(rowId)
    val paddedH = roundUp(toeplitzed.size, Ih).toInt
    val paddedW = roundUp(toeplitzed(0).size, Iw).toInt
    pad2D(toeplitzed, paddedH, paddedW, BR)
  }

  // 行间padding
  def inputValue(rowId: Int) = {
    val slided = inputToeplitzedPadded(rowId).map(_.sliding(Iw, Iw).toArray).sliding(Ih, Ih).toArray
    val transposed = slided.map(transpose3D(_))
    transposed.flatten.map(_.flatten)
  }

  def weightValue = {
    val slided = weightMatrix.map(_.sliding(Ww, Ww).toArray).sliding(Wh, Wh).toArray
    val transposed = slided.map(transpose3D(_))
    transposed.flatten.map(_.flatten)
  }

  def outputValue(rowId: Int) = { // todo 对output进行padding
    val slided = outputMatrix(rowId: Int).map(_.sliding(Iw, Iw).toArray).sliding(Wh, Wh).toArray
    val transposed = slided.map(transpose3D(_))
    transposed.flatten.map(_.flatten)
  }

  def inputPE(rowId: Int, PEId: Int) = inputValue(rowId).slice(PEId * Ih, (PEId + 1) * Ih)

  def weightPE(PEId: Int) = weightValue.map(_.slice(PEId * Ih, (PEId + 1) * Ih))
}

object ToeplitzConv {
  def main(args: Array[String]): Unit = {

    import projects.Hwj._

    val pre = res3_2_b
    val current = res3_2_c

    val memGrid = Array(100, 10000)

    val inputMemMap = (index: Index) => {
      val c = index.coords(0)
      val h = index.coords(1)
      val w = index.coords(2)
      Index(memGrid, w, c)
    }


    println(pre.outputValue(0).map(_.map(_.coord1D)).flatten.distinct.size)
    println(current.inputValue(0).map(_.map(_.coord1D).filter(_ >= 0)).flatten.distinct.size)
    val grouped = getGroups(pre.outputValue(0).map(_.map(_.coord1D)) ++ current.inputValue(0).map(_.map(_.coord1D)))
    val addrMap = getMap(grouped)
    printArray2D(current.inputValue(0).map(_.map(index => if (index.coord1D != -1) addrMap(index.coord1D) else (-1, -1))))
    printArray2D(pre.outputValue(0).map(_.map(index => addrMap(index.coord1D))))
    println("port " + grouped.size)
    println("port depth")
  }
}
