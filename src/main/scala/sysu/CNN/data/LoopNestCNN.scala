package sysu.CNN.data

/*
design : 目前采用的简化设定
  Padding= same
  Stride = 1
    thus, Hout = Hin
*/
case class LoopNestCNN(
                        Nof: Int = 64,
                        Nif: Int = 64,
                        Nix: Int = 32,
                        Niy: Int = 32,
                        Nkx: Int = 1,
                        Nky: Int = 1,
                        Noy: Int = 64,
                        Nox: Int = 64,
                        Tof: Int = 64,
                        Tif: Int = 64,
                        Tix: Int = 32,
                        Tiy: Int = 32,
                        Tkx: Int = 1,
                        Tky: Int = 1,
                        Tox: Int = 32,
                        Toy: Int = 32,
                        Pif: Int = 16,
                        Pof: Int = 16
                      ) {
  def format(s: String, width: Int) = s.padTo(width, " ").mkString("")

  def printArray2D[T](array: Array[Array[T]], width: Int = 5, head: Boolean = true) = {
    if (head) println(array.dropRight(array.length - 100).map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
    else println(array.map(_.map(ele => format(ele.toString, width)).mkString(" ")).mkString("\n"))
  }

  // CNN通用
  val inputMaps = Array.tabulate(Nif, Niy, Nix)((c, h, w) => c * Niy * Nix + h * Nix + w)
  val weightMaps = Array.tabulate(Nof, Nif, Nky, Nkx)((n, c, h, w) => n * Nif * Nky * Nkx + c * Nky * Nkx + h * Nkx + w)
  val outputMaps = Array.tabulate(Nof, Noy, Nox)((n, h, w) => n * Noy * Nox + h * Nox + w)

  val inputGrid = Array(Nif, Niy, Nix)
  val weightGrid = Array(Nof, Nif, Nky, Nkx)
  val outputGrid = Array(Nif, Niy, Nix)
  val inputIndex = (c: Int, h: Int, w: Int) => Index(inputGrid, c, h, w)
  val weightIndex = (n: Int, c: Int, kh: Int, kw: Int) => Index(weightGrid, n, c, kh, kw)
  val outputIndex = (n: Int, h: Int, w: Int) => Index(outputGrid, n, h, w)

  // 依照并行度设置和变换函数,得到一个周期的数据


  // order = kx, ky, ni, ox, oy, no (same as Ma,2018)
  // template val transform : transformerXX = (kx:Int, ky:Int, c:Int, ox:Int, oy:Int, n:Int) => {}
  type transformer = (Int, Int, Int, Int, Int, Int) => Any

  // 特定展开,Huang
  def GeneratorHuang[T](transformer: (Int, Int, Int, Int, Int, Int) => T) = {
    for (oy <- 0 until Toy; ox <- 0 until Tox; n <- 0 until Tof by Pof; c <- 0 until Tif by Pif; ky <- 0 until Tky; kx <- 0 until Tkx) yield {
      transformer(kx, ky, c, ox, oy, n)
    }
  }

  //  val inputAccess = (kx: Int, ky: Int, c: Int, ox: Int, oy: Int, n: Int) =>
  //    (0 until Pif).map(i => inputMaps(c + i)(oy + ky - Nky / 2)(ox + kx - Nkx / 2)).toArray // todo : 改成真正通用的方法,在这之前,先确定传递loop order的方法
  val inputAccess = (kx: Int, ky: Int, c: Int, ox: Int, oy: Int, n: Int) =>
    (0 until Pif).map(i => inputIndex(c + i, oy + ky - Nky / 2, ox + kx - Nkx / 2)).toArray // todo : 改成真正通用的方法

  val weightAccess = (kx: Int, ky: Int, c: Int, ox: Int, oy: Int, n: Int) => {
    (for (i <- 0 until Pof; j <- 0 until Pif) yield {
      weightIndex(n + i, c + j, ky, kx)
    }).toArray
  }

  val outputAccess = (kx: Int, ky: Int, c: Int, ox: Int, oy: Int, n: Int) =>
    (0 until Pof).map(i => outputIndex(n + i, oy, ox)).toArray

  def inputValueHuang() = GeneratorHuang(inputAccess).toArray

  def outputValueHuang() = GeneratorHuang(outputAccess).toArray

  // design : 对于这个具体的设计,64个输入通道上的input以cyclic方式存在16个port(16个RAM18)
  // todo : 建立一种方法描述存储layout
  def inputAddrMapHuang(index: Index) = {
    val c = index.coords(0)
    val h = index.coords(1)
    val w = index.coords(2)
    val port18 = c % Pif
    val addr18 = h * Tix * 4 + w * 4 + c / Pif % (Nif / Pif)
    if(c >= 0 && h >= 0 && w >= 0) (port18, addr18) else (-1, -1)
  }

  def outputAddrMapHuang(index: Index) = {
    val n = index.coords(0)
    val h = index.coords(1)
    val w = index.coords(2)
    val port18 = n % 16
    val addr18 = h * Tix * 4 + w * 4 + n / Pif % (Nif / Pif)
    (port18, addr18)
  }

  def inputAddrHuang() = GeneratorHuang(inputAccess).toArray.map(_.map(inputAddrMapHuang(_)))

  def outputAddrHuang = GeneratorHuang(outputAccess).toArray.map(_.map(outputAddrMapHuang(_)))

  /*
  design : 对于这个具体设计
    64个输出通道上的input以cyclic方式存在16组port,每组4个port(64个RAM18)
    64个输入通道上的input位宽拼接为8个输入通道,以cyclic方式存在同组内的4个port
   */
  // design : 在规整的访存情景下,实现两个个RAM18而非一个RAM36以复用地址会更简单
  def weightAddrMapHuang(n: Int, c: Int, h: Int, w: Int, param: LoopNestCNN) = {
    val port18 = n % 16 * 4 + c / 4 % 4
    val addr18 = n / 4 + c / 4 / 4
    (port18, addr18)
  }
}

object LoopNestCNN { // demo
  def main(args: Array[String]): Unit = {

    val LN = LoopNestCNN()

    //    println(LN.inputValueHuang().length)
    //    println(LN.inputValueHuang()(0).length)
    LN.printArray2D(LN.outputValueHuang(), 10)
    LN.printArray2D(LN.outputAddrHuang, 10)

    val LNNext = LoopNestCNN(Nkx = 3, Nky = 3, Tkx = 3, Tky = 3)

    LNNext.printArray2D(LNNext.inputValueHuang(), 10)
    LNNext.printArray2D(LNNext.inputAddrHuang(), 10)
    //    println(LN.outputValueHuang().length)
    //    println(LN.outputValueHuang()(0).length)
    //    LN.printArray2D(LN.inputAddrHuang())

    //    println(LN.inputIndex(1, 2, 3).coord1D)


    import sysu.xilinx._
    import sysu.util._

    // 生成input访存序列发生器
    //    val inputReport = VivadoFlow(
    //      design = new SeqGen(inputAddrSeq),
    //      vivadoConfig = recommended.vivadoConfig,
    //      vivadoTask = VivadoTask(
    //        topModuleName = "SeqGenHuang",
    //        workspacePath = "output/huang/input1SeqGen"),
    //      force = true
    //    ).doit()

    //    inputReport.printArea
    //    inputReport.printFMax
  }
}
