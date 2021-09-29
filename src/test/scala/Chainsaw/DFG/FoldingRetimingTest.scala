package Chainsaw.DFG

import org.scalatest.flatspec.AnyFlatSpec

class FoldingRetimingTest extends AnyFlatSpec {

  behavior of "FoldingRetimingTest"

  val dfg = DFG() // fig6.3
  val adds = (0 until 4).map(i => AbstractNode(delayv = 1, executionTimev = 1, name = s"add$i"))
  val mults = (0 until 4).map(i => AbstractNode(delayv = 1, executionTimev = 1, name = s"mult$i"))

  val Seq(a0, a1, a2, a3) = adds
  val Seq(m0, m1, m2, m3) = mults

  (adds ++ mults).foreach(node => dfg.addVertex(node))

  val addFoldingSet = Seq(a3, a1, a2, a0)
  val multFoldingSet = Seq(m0, m3, m1, m2)

  val dataIn = dfg.addInput()
  val inputFoldingSet = Seq(dataIn, null, null, null)

  dfg.addVertexFromSources(Seq(dataIn, a2), a0, Seq(0, 0))
  dfg.addVertexFromSources(Seq(a0, a3), a1, Seq(1, 0))
  dfg.addVertexFromSources(Seq(m0, m2), a2, Seq(1, 0))
  dfg.addVertexFromSources(Seq(m1, m3), a3, Seq(1, 1))

  dfg.addVertexFromSource(a0, m0, 1)
  dfg.addVertexFromSource(a0, m1, 1)
  dfg.addVertexFromSource(a0, m2, 1)
  dfg.addVertexFromSource(a0, m3, 2)

  //  dfg.setInput()

  val algo = new FoldingRetiming(dfg, Seq(addFoldingSet, multFoldingSet, inputFoldingSet))

  val add2 = DFG()
  val add0, add1 = AbstractNode(delayv = 0, executionTimev = 1)
  add2.addVertex(add0)
  add2.addVertex(add1)
  add2.addEdge(add0, add1, 0)

  val simpleFolding = Seq(add0, add1)
  val simpleAlgo = new FoldingRetiming(add2, Seq(simpleFolding))

  val fft4 = DFG()
  val butterflies = (0 until 4).map(i => AbstractNode(1, 1, s"b$i"))
  val Seq(butterfly0, butterfly1, butterfly2, butterfly3) = butterflies

  fft4.addVertices(butterflies)
  fft4.addEdge(butterfly0, butterfly2, 0)
  fft4.addEdge(butterfly1, butterfly3, 0)
  fft4.addEdge(butterfly0, butterfly3, 0)
  fft4.addEdge(butterfly1, butterfly2, 0)

  val fft4Set = Seq(Seq(butterfly0, butterfly1), Seq(butterfly2, butterfly3))
  val fft4Algo = new FoldingRetiming(fft4, fft4Set)


  it should "solveRetiming" in {

    //    println(algo.solveRetiming())
    //    println(simpleAlgo.solveRetiming())
    println(fft4Algo.solveRetiming().mkString(" "))

  }

  it should "foldingOrders" in {

  }

}
