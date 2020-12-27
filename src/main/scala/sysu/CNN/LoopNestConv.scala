package sysu.CNN

import scala.collection.mutable.Map

/*
design : 目前采用的简化设定
  Padding= same
  Stride = 1
    thus, Hout = Hin
*/

trait LoopVar {
  val name = ""
  val report : Map[String, Array[Int]] = Map()

  def + (that:LoopVar) = {
    this.report ++= that.report
    this
  }

  def % (number : Int) = {
    report(name)(0) = number
    this
  }

  def / (number: Int) = {
    report(name)(1) = number
    this
  }
}

case object OY extends LoopVar {
  override val name = "oy"
  override val report = Map("oy" -> Array(0, 0))
}

case object OX extends LoopVar {
  override val name = "ox"
  override val report = Map("ox" -> Array(0, 0))
}
case object OF extends LoopVar // n
case object IF extends LoopVar // c
case object KY extends LoopVar // ky
case object KX extends LoopVar // kx

case class DimensionParam(
                           val N: Int,
                           val T: Int,
                           val P: Int
                         )

case class LoopNestConv(
                         Nof: Int = 64, Nif: Int = 64, Niy: Int = 32, Nix: Int = 32, Nky: Int = 1, Nkx: Int = 1,
                         Tof: Int = 64, Tif: Int = 64, Tiy: Int = 32, Tix: Int = 32, Tky: Int = 1, Tkx: Int = 1,
                         Pif: Int = 16, Pof: Int = 16, Pky: Int = 1, Pkx: Int = 1, Poy: Int = 1, Pox: Int = 1,
                         Stride: Int = 1,
                         tileOrder: Array[LoopVar] = Array(KX, KY, IF, OF, OX, OY),
                         outerOrder: Array[LoopVar] = Array(KX, KY, IF, OF, OX, OY)
                       ) {
  require(Nix == Niy && Nkx == Nky) // todo : 支持非正方形
  require(Nkx % 2 == 1)
  require((Nix - Nkx) % Stride == 0)
  require((Tix - Nkx) % Stride == 0)
  val Nox = (Nix - Nkx) / Stride + 1
  val Noy = (Niy - Nky) / Stride + 1
  val Tox = (Tix - Nkx) / Stride + 1
  val Toy = (Tiy - Nky) / Stride + 1


  def format(s: String, width: Int) = s.padTo(width, " ").mkString("")

  def printArray2D[T](array: Array[Array[T]], width: Int = 5, head: Boolean = true, tail: Boolean = false) = {
    if (tail) println(array.drop(array.length - 100).map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
    else if (head) println(array.dropRight(array.length - 100).map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
    else println(array.map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
  }

  // CNN通用
  val inputGrid = Array(Nif, Niy, Nix)
  val weightGrid = Array(Nof, Nif, Nky, Nkx)
  val outputGrid = Array(Nif, Niy, Nix)
  val inputIndex = (c: Int, h: Int, w: Int) => Index(inputGrid, c, h, w)
  val weightIndex = (n: Int, c: Int, kh: Int, kw: Int) => Index(weightGrid, n, c, kh, kw)
  val outputIndex = (n: Int, h: Int, w: Int) => Index(outputGrid, n, h, w)

  val numPE = Pkx * Pky * Pif * Pof * Pox * Poy
  val inputSize = inputGrid.reduce(_ * _)
  val weightSize = weightGrid.reduce(_ * _)
  val outputSize = outputGrid.reduce(_ * _)

  // 用于行缓存参考
  val inputRowSize = inputSize / Nix
  val weightRowSize = weightSize
  val outputRowSize = outputSize / Nox

  // 依照并行度设置和变换函数,得到一个周期的数据
  // order = kx, ky, ni, no, ox, oy
  // template val transform : transformerXX = (kx:Int, ky:Int, c:Int, n:Int, ox:Int, oy:Int) => {}
  type transformer = (Int, Int, Int, Int, Int, Int) => Any

  def getDimensionParam(loopVar: LoopVar) = {
    loopVar match {
      case OF => DimensionParam(Nof, Tof, Pof)
      case IF => DimensionParam(Nif, Tif, Pif)
      case OY => DimensionParam(Noy, Toy, Poy)
      case OX => DimensionParam(Nox, Tox, Pox)
      case KY => DimensionParam(Nky, Tky, Pky)
      case KX => DimensionParam(Nkx, Tkx, Pkx)
    }
  }

  // todo : 实现一个tile的基地址
  def traverseTile[T](transformer: (Int, Int, Int, Int, Int, Int) => T) = {

    // 处理实际遍历顺序和默认access顺序之间的转换
    def reOrderTile(varArray: Array[Int]) = {
      val result = Array.ofDim[Int](6)
      result(tileOrder.indexOf(KX)) = varArray(0)
      result(tileOrder.indexOf(KY)) = varArray(1)
      result(tileOrder.indexOf(IF)) = varArray(2)
      result(tileOrder.indexOf(OF)) = varArray(3)
      result(tileOrder.indexOf(OX)) = varArray(4)
      result(tileOrder.indexOf(OY)) = varArray(5)
      result
    }

    val params = tileOrder.map(getDimensionParam(_))
    for (var5 <- 0 until params(5).T by params(5).P;
         var4 <- 0 until params(4).T by params(4).P;
         var3 <- 0 until params(3).T by params(3).P;
         var2 <- 0 until params(2).T by params(2).P;
         var1 <- 0 until params(1).T by params(1).P;
         var0 <- 0 until params(0).T by params(0).P)
      yield {
        val vars = reOrderTile(Array(var0, var1, var2, var3, var4, var5))
        transformer(vars(0), vars(1), vars(2), vars(3), vars(4), vars(5))
      }
  }

  def rangeSafe(kx: Int, ky: Int, c: Int, n: Int, ox: Int, oy: Int) = {
    (0 <= kx && kx < Nkx) &&
      (0 <= ky && ky < Nky) &&
      (0 <= c && c < Nif) &&
      (0 <= n && n < Nof) &&
      (0 <= ox && ox < Nox) &&
      (0 <= oy && oy < Noy)
  }

  val inputAccess = (kx: Int, ky: Int, c: Int, n: Int, ox: Int, oy: Int) => {
    (for (var5 <- 0 until Poy; var4 <- 0 until Pox; var2 <- 0 until Pif; var1 <- 0 until Pky; var0 <- 0 until Pkx) yield {
      val _c = c + var2
      val _oy = oy + var5 + ky + var1 - Nky / 2
      val _ox = ox + var4 + kx + var0 - Nkx / 2
      inputIndex(_c, _oy, _ox)
      // todo : 也许不做任何处理更好
      //      if (rangeSafe(kx = 0, ky = 0, c = _c, n = 0, ox = _ox, oy = _oy)) inputIndex(_c, _oy, _ox) else inputIndex(-1, -1, -1)
    }).toArray
  }

  val weightAccess = (kx: Int, ky: Int, c: Int, n: Int, ox: Int, oy: Int) => {
    (for (var3 <- 0 until Pof; var2 <- 0 until Pif; var1 <- 0 until Pky; var0 <- 0 until Pkx) yield {
      weightIndex(n + var3, c + var2, ky + var1, kx + var0)
    }).toArray
  }

  val outputAccess = (kx: Int, ky: Int, c: Int, n: Int, ox: Int, oy: Int) => {
    (for (var5 <- 0 until Poy; var4 <- 0 until Pox; var3 <- 0 until Pof) yield {
      outputIndex(n + var3, oy + var5, ox + var4)
    }).toArray
  }

  def inputValue() = traverseTile(inputAccess).toArray

  def weightValue() = traverseTile(weightAccess).toArray

  def outputValue() = traverseTile(outputAccess).toArray

  def inputAddr(addrMap: Index => (Int, Int)) = traverseTile(inputAccess).toArray.map(_.map(addrMap(_)))

  def outputAddr = traverseTile(outputAccess).toArray.map(_.map(outputAddrMapHuang(_)))

  // 特定展开,Huang
  //  def GeneratorHuang[T](transformer: (Int, Int, Int, Int, Int, Int) => T) = {
  //    for (oy <- 0 until Toy; ox <- 0 until Tox; n <- 0 until Tof by Pof; c <- 0 until Tif by Pif; ky <- 0 until Tky; kx <- 0 until Tkx) yield {
  //      transformer(kx, ky, c, n, ox, oy)
  //    }
  //  }

  def outputAddrMapHuang(index: Index) = {
    val n = index.coords(0)
    val h = index.coords(1)
    val w = index.coords(2)
    val port18 = n % 16
    val addr18 = h * Tix * 4 + w * 4 + n / Pif % (Nif / Pif)
    (port18, addr18)
  }

  /*
  design : 对于这个具体设计
    64个输出通道上的input以cyclic方式存在16组port,每组4个port(64个RAM18)
    64个输入通道上的input位宽拼接为8个输入通道,以cyclic方式存在同组内的4个port
   */
  // design : 在规整的访存情景下,实现两个个RAM18而非一个RAM36以复用地址会更简单
  def weightAddrMapHuang(n: Int, c: Int, h: Int, w: Int) = {
    val port18 = n % 16 * 4 + c / 4 % 4
    val addr18 = n / 4 + c / 4 / 4
    (port18, addr18)
  }
}

object LoopNestConv { // demo
  def main(args: Array[String]): Unit = {

    val Huang1 = LoopNestConv(Nkx = 3, Nky = 3, Tkx = 3, Tky = 3, Pof = 32)
    import Huang1._
    // design : 对于这个具体的设计,64个输入通道上的input以cyclic方式存在16个port(16个RAM18)
    // todo : 建立一种方法描述存储layout
    // todo : 将inputAddrMap作为一个实现为一个需要实现的属性 类型为(Int, Int, Int) => (Int, Int)
    val inputAddrMapHuang = (index: Index) => {
      val c = index.coords(0)
      val h = index.coords(1)
      val w = index.coords(2)
      val port18 = c % Pif
      val addr18 = h * Tix * 4 + w * 4 + c / Pif % (Nif / Pif)
      (port18, addr18)
      // todo : 也许不做任何处理更好
      //      if (c >= 0 && h >= 0 && w >= 0) (port18, addr18) else "padding"
    }

    printArray2D(huang.loopNestHuang2.inputAddr(inputAddrMapHuang), 10, tail = true)
    //    println(huang.loopNestHuang2.inputAddr(inputAddrMapHuang).length)
    //    // 将自己找出此序列的过程变为算法
    //    val seq = (0 until 73728).map(i => i % 3 * 4 + i % 9 / 3 * 128 + i % 36 / 9 + i / 72 * 4 + (-132))
    //    val compare = huang.loopNestHuang2.inputAddr(inputAddrMapHuang).map(_ (0)._2).zip(seq).filter { case (my, truth) => my != truth }
    //    println(seq.length - compare.length)
    //    compare.dropRight(compare.length - 100).foreach(println(_))

  }
}
