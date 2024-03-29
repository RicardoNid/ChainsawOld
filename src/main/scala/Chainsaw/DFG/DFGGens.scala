package Chainsaw.DFG

import Chainsaw._
import Chainsaw.DFG._
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
import scala.math.{abs, pow}
import Operators._

import scala.collection.mutable.ArrayBuffer

// Architecutre selections
object FirArch extends Enumeration {
  type FirArch = Value
  val DIRECT, TRANSPOSE, SYSTOLIC = Value
}

object FirType extends Enumeration {
  type FirType = Value
  val STATIC, DYNAMIC = Value
}

object FFTArch extends Enumeration {
  type FFTArch = Value
  val DIT, DIF = Value
}

import Chainsaw.DFG.FFTArch._
import Chainsaw.DFG.FirArch._

// TODO: more functions and transformations on FIR, e.g:
//    multiphase decomposition
//    MIMO(for example, for convenc)
// TODO: move the parallelism implementation outside of the DFG
class FIRGen[THard <: Data, TSoft](mac: TrinaryNode[THard], firArch: FirArch, coeffs: Seq[TSoft], coeffWidth: BitCount, parallelism: Int)(implicit
    converter: (TSoft, BitCount) => THard
) extends DFGGen[THard] {

  def getGraph: DFGGraph[THard] = {
    val dfg  = DFGGraph[THard](s"${firArch}_fir_using_${mac.name}")
    val size = coeffs.size

    // adding nodes to the graph
    val macs   = (0 until size).map(i => mac.copy(s"${mac.name}_$i"))
    val consts = coeffs.zipWithIndex.map { case (soft, i) => ConstantNode[THard, TSoft](s"const_$i", soft, coeffWidth) }
    val zero   = ConstantNode[THard, TSoft](s"zero", 0.asInstanceOf[TSoft], coeffWidth) // TODO: not generic
    (macs ++ consts :+ zero).foreach(dfg.addVertices(_))
    val input = dfg.addInput("input")

    /** build the fir graph
      */
    def buildFir(a: Int, b: Int): Unit = {
      val coeffs = if (firArch == TRANSPOSE) consts else consts.reverse
      macs.zip(coeffs).foreach { case (multAdd, coeff) => dfg.addEdge(coeff(0), multAdd(0), 0) } // mac port 0, allocate coefficients to mac units
      macs.zipWithIndex.foreach { case (mult, i) => dfg.addEdge(input(0), mult(1), a * i) } // mac port 1, connecting the input delay line to mac units
      // mac port 2, connecting macs, the first mac unit takes a zero
      macs.init.zip(macs.tail).foreach { case (prev, next) => dfg.addEdge(prev(0), next(2), b) }
      dfg.addEdge(zero(0), macs.head(2), b)
    }

    firArch match {
      case DIRECT    => buildFir(1, 0)
      case TRANSPOSE => buildFir(0, 1)
      case SYSTOLIC  => buildFir(2, 1)
    }
    dfg.setOutput(macs.last)
    dfg.setLatency(latency)

    val multAddGroups =
      if (parallelism < 0) macs.grouped(-parallelism).toSeq.map(_.padTo(-parallelism, null))
      else null
    val ret = dfg.parallelized(parallelism, multAddGroups)
    logger.debug(s"fir dfg:\n$ret")
    ret
  }

  def latency: Int = (if (firArch == SYSTOLIC) coeffs.size - 1 else 0) + mac.delay

  def getGraphAsNode(dataReset: Boolean = true)(implicit holderProvider: HolderProvider[THard]): DSPNode[THard] = {
    val graph = getGraph
    graph.asNode(graph.name, graphLatency = latency cycles, dataReset)
  }
}

object FIRGen {

  /** generic method to generate an abstract FIR DFG
    *
    * @param mac
    *   abstract multiply-add node
    */
  def apply[THard <: Data, TSoft](mac: TrinaryNode[THard], firArch: FirArch, coeffs: Seq[TSoft], coeffWidth: BitCount, parallelism: Int)(implicit
      converter: (TSoft, BitCount) => THard
  ): FIRGen[THard, TSoft] =
    new FIRGen(mac, firArch, coeffs, coeffWidth, parallelism)(converter)

  /** using add & mult, rather than a MAC as building blocks to implement FIR
    *
    * @param add
    *   commutative adder node(2->1)
    * @param mult
    *   multiplier node, port0 for coeff, port1 for data(2->1)
    *
    * using add and mult, we can build a multAddNode as the building block of our design
    */
  def apply[THard <: Data, TSoft](
      add: BinaryNode[THard],
      mult: BinaryNode[THard],
      firArch: FirArch,
      coeffs: Seq[TSoft],
      coeffWidth: BitCount,
      parallelism: Int
  )(implicit converter: (TSoft, BitCount) => THard): FIRGen[THard, TSoft] = {
    // building mac by add & mult
    val multAddOp = (a: THard, b: THard, c: THard) => {
      val prod = mult.impl(Seq(a, b), null).head
      val sum  = add.impl(Seq(Delay(c, mult.delay), prod), null).head
      sum
    }
    val mac = TrinaryHardware(multAddOp, width = add.outWidths.head, delay = (add.delay + mult.delay) cycles).asDeviceNode(s"${add.name}_${mult.name}")
    new FIRGen(mac, firArch, coeffs, coeffWidth, parallelism)(converter)
  }
}

// TODO: cannot infer TSoft from the coeffGen, why?

/** Generic method to generate an abstract radix-2 FFT DFG, this method use a "butterfly core" as its building block
  *
  * @param ctButterfly
  *   : the implementation of the butterfly core
  * @param size
  *   : size of the FFT DFG, should be a power of 2
  * @param fftArch
  *   : architecture of fft, could be DIT or DIF
  * @param coeffGen
  *   : a generator to generate the twiddle factor w^ik^ on the specific field
  * @param coeffWidth
  *   : bit width of the coefficients
  * @param parallelism
  *   : determining the parallelism of the design, compared to the fully-pipelined version
  * @example
  *   parallelism = 3 -> unfold 3; parallelism = -3 -> fold 3
  */
class ButterflyGen[THard <: Data, TSoft](
    ctButterfly: ButterflyNode[THard],
    gsButterfly: ButterflyNode[THard],
    size: Int,
    fftArch: FFTArch,
    inverse: Boolean = false,
    coeffGen: Int => TSoft,
    coeffWidth: BitCount,
    parallelism: Int = 1
)(implicit converter: (TSoft, BitCount) => THard)
    extends DFGGen[THard] {

  def getGraph: DFGGraph[THard] = {
    logger.info(s"start generating butterfly graph of size $size")
    require(isPow2(size))
    require(gsButterfly.delay == ctButterfly.delay)
    val dfg = DFGGraph[THard]("butterfly_graph")

    val inputs = (0 until size).map(i => dfg.addInput(s"input$i"))

    logger.debug(s"coeffs in dfg ${(0 until size / 2).map(i => coeffGen(if (inverse) -i else i)).mkString(" ")}")
    val consts = (0 until size / 2).map { i =>
      val index = if (inverse) -i else i
      // we us the absolute value of index as "^" or "-" are not allowed in verilator
      ConstantNode[THard, TSoft](s"omega_${abs(index)}", coeffGen(index), coeffWidth) // coefficients
    }
    dfg.addVertices(consts: _*)

    val butterflies = Seq.tabulate(log2Up(size), size / 2) { (stage, index) =>
      fftArch match {
        case DIT => ctButterfly.copy(s"butterfly${stage}_$index")
        case DIF => gsButterfly.copy(s"butterfly${stage}_$index")
      }
    }
    dfg.addVertices(butterflies.flatten: _*)

    val N = size

    def buildStage(dataIns: Seq[DSPNodeWithOrder[THard]], stage: Int, count: Int): Seq[DSPNodeWithOrder[THard]] = {
      logger.debug(s"input to stage: ${dataIns.mkString(" ")}")
      val (up, down) = dataIns.splitAt(dataIns.size / 2)
      val step       = N / dataIns.size
      val temp = up.zip(down).zipWithIndex.map { case ((u, v), i) =>
        val coeff     = consts(step * i) // constant node
        val butterfly = butterflies(stage)(count * dataIns.size / 2 + i)

        logger.debug(s"connecting ${butterfly.name}")
        dfg.addEdge(u, butterfly(0), 0)
        dfg.addEdge(v, butterfly(1), 0)
        dfg.addEdge(coeff, butterfly(2), 0)

        (butterfly(0), butterfly(1))
      }

      val ret = temp.map(_._1) ++ temp.map(_._2)
      logger.debug(s"output from stage: ${ret.mkString(" ")}")
      ret
    }

    def buildRecursively(dataIns: Seq[DSPNodeWithOrder[THard]], stage: Int, count: Int): Seq[DSPNodeWithOrder[THard]] = {
      if (dataIns.size == 1) dataIns
      else {
        if (fftArch == DIF) {
          val temp       = buildStage(dataIns, stage, count)
          val (up, down) = temp.splitAt(dataIns.size / 2)
          buildRecursively(up, stage + 1, count * 2) ++ buildRecursively(down, stage + 1, count * 2 + 1)
        } else { // fftArch == DIT
          val (up, down) = dataIns.splitAt(dataIns.size / 2)
          val temp       = buildRecursively(up, stage - 1, count * 2) ++ buildRecursively(down, stage - 1, count * 2 + 1)
          buildStage(temp, stage, count)
        }
      }
    }

    val ret = fftArch match {
      case DIT => buildRecursively(inputs.map(DSPNodeWithOrder(_, 0)), log2Up(size) - 1, 0)
      case DIF => buildRecursively(inputs.map(DSPNodeWithOrder(_, 0)), 0, 0)
    }

    ret.foreach(port => dfg.setOutput(port.node, port.order))
    dfg.setLatency(latency)

    val butterflyGroups: Seq[Seq[ButterflyNode[THard]]] =
      if (parallelism < 0) butterflies.map(col => col.grouped(-parallelism).toSeq).flatten
      else null
    val parallelizedDFG = dfg.parallelized(parallelism, butterflyGroups)

    if (parallelism < 0)
      DFGTestUtil.verifyFolding[UInt](
        dfg.asInstanceOf[DFGGraph[UInt]],
        butterflyGroups.asInstanceOf[Seq[Seq[DSPNode[UInt]]]],
        UInt(12 bits),
        "testFoldedButterfly"
      )

    logger.debug(s"butterfly dfg:\n$dfg")
    parallelizedDFG
  }

  override def latency: Int = log2Up(size) * gsButterfly.delay
}

object ButterflyGen {
  def apply[THard <: Data, TSoft](
      ctButterfly: ButterflyNode[THard],
      gsButterfly: ButterflyNode[THard],
      size: Int,
      fftArch: FFTArch,
      inverse: Boolean,
      coeffGen: Int => TSoft,
      coeffWidth: BitCount,
      parallelism: Int
  )(implicit converter: (TSoft, BitCount) => THard): ButterflyGen[THard, TSoft] =
    new ButterflyGen(ctButterfly, gsButterfly, size, fftArch, inverse, coeffGen, coeffWidth, parallelism)(converter)
}

// LQX: implement this
class BinaryTreeGen[T <: Data](binaryNode: BinaryNode[T], size: Int) extends DFGGen[T] {

  /** @return
    *   the binaryTree graph we build
    */
  override def getGraph: DFGGraph[T] = {
    val dfg = DFGGraph[T](s"binaryTree_using_${binaryNode.name}")
    logger.info(s"start generating binaryTree graph of size $size")
    if (!isPow2(size + 1)) logger.info("this binaryTree is not a full-binaryTree")

    /** the inter-layer op for building binaryTree
      * @param upLayerNodes
      *   it is the level layer nodes(seq) for connection when we connecting level layer and level +1 layer
      * @param downLayerNodes
      *   it is the level + 1 layer nodes(seq) for connection when we connecting level layer and level +1 layer
      * @param level
      *   the level of connection which means we connecting the level layer and level + 1 layer
      */
    def interLayerOp(upLayerNodes: Seq[BinaryNode[T]], downLayerNodes: Seq[BinaryNode[T]], level: Int): Unit = {
      require(upLayerNodes.size == pow(2, level).toInt, "the number of uplayernodes is mismatch the level")

      // pre-process for downLayerNodes
      val evenIndexDownNodes  = downLayerNodes.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
      val groupDownLayerNodes = evenIndexDownNodes.zipAll(downLayerNodes.diff(evenIndexDownNodes), null, null)
      // connecting nodes
      groupDownLayerNodes.zip(upLayerNodes).foreach { case ((cn1, cn2), ln) =>
        Seq(cn1, cn2).filterNot(_ == null).zipWithIndex.foreach { case (cn, id) =>
          dfg.addEdge(cn(0), ln(id), 0)
        }
      }
    }

    // according to the size ; build the nodes and add it to dfg
    val treeNodes = Seq.tabulate(size)(i => binaryNode.copy(s"${binaryNode.name}${i + 1}"))
    treeNodes.foreach(dfg.addVertex)
    // use the op above to construct graph
    val levelCount = log2Up(size + 1) - 1
    (0 until levelCount).foreach { level =>
      // decide the layer-nodes
      val indexCond    = pow(2, level + 2).toInt - 1 >= size
      val upLayerNodes = treeNodes.slice(pow(2, level).toInt - 1, pow(2, level + 1).toInt - 1)
      val downLayerNodes = indexCond match {
        case true  => treeNodes.slice(pow(2, level + 1).toInt - 1, size)
        case false => treeNodes.slice(pow(2, level + 1).toInt - 1, pow(2, level + 2).toInt - 1)
      }

      interLayerOp(upLayerNodes, downLayerNodes, level)
      // set the IO port
      if (level == 0) {
        dfg.setOutput(upLayerNodes(0), 0)
      }
      if (level == levelCount - 1) {
        val paddingNodes = downLayerNodes ++ Seq.fill(pow(2, level + 1).toInt - downLayerNodes.size)(null)

        val layerMap = upLayerNodes.zipWithIndex.map { case (node, id) => node -> paddingNodes.slice(2 * id, 2 * id + 2) }
        layerMap.foreach { case (ln, pns) =>
          pns.foreach(pn =>
            if (pn == null) dfg.setInput(ln, dfg.incomingEdgesOf(ln).size)
            else (0 until 2).foreach(i => dfg.setInput(pn, i))
          )
        }
      }
    }
    dfg
  }

  override def latency: Int = 0
}

object BinaryTreeGenTest extends App {
  val node = BinaryHardware(sintAdd).asDeviceNode("testnode")
  val dfg  = new BinaryTreeGen(node, 9)
  println(dfg.getGraph)
}
// LQX: implement this, after the implementation, an example of corresponding adder and its test should be implemented in "comparith package"

/** hybrid BrentKung/KoggeStone parallel prefix graph
  *
  * @see
  *   ''COMPUTER ARITHMETIC: Algorithms and Hardware Designs, Behrooz Parhami'', chapter 6.5
  * @see
  *   [[https://en.wikipedia.org/wiki/Brent%E2%80%93Kung_adder]]
  * @see
  *   [[https://en.wikipedia.org/wiki/Kogge%E2%80%93Stone_adder]]
  * @see
  *   my implementation [[DFG.BKKSTree]]
  */
class BKKSTreeGen[T <: Data](binaryNode: BinaryNode[T], size: Int) extends DFGGen[T] {
  override def getGraph: DFGGraph[T] = {
    val dfg = DFGGraph[T](s"bkksTree_using_${binaryNode.name}")

    dfg
  }

  override def latency: Int = 0
}

// LQX: implement this, the node is not binary, try to fix it

/** Wallace/Dadda tree graph whose building blocks are 3-2 compressors and inputs are "bit"s, rather than words
  *
  * @see
  *   ''COMPUTER ARITHMETIC: Algorithms and Hardware Designs, Behrooz Parhami'', chapter 8.3
  * @see
  *   [[https://en.wikipedia.org/wiki/Wallace_tree]]
  * @see
  *   [[https://en.wikipedia.org/wiki/Dadda_multiplier]]
  */
class WallaceTreeGen[T <: Data](binaryNode: BinaryNode[T], size: Int) extends DFGGen[T] {
  override def getGraph: DFGGraph[T] = {
    val dfg = DFGGraph[T](s"bkksTree_using_${binaryNode.name}")

    dfg
  }

  override def latency: Int = 0
}

// LQX: implement this by transplanting implementations from MCM and Architectures
// generally speaking, this method should be able to generate a minimized adder graph according to a given integer
class AdderTree[T <: Data](binaryNode: BinaryNode[T], size: Int) extends DFGGen[T] {
  override def getGraph: DFGGraph[T] = {
    val dfg = DFGGraph[T](s"adderTree")

    dfg
  }

  override def latency: Int = 0
}
