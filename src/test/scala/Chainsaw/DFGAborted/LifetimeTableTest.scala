package Chainsaw.DFGAborted

import org.scalatest.flatspec.AnyFlatSpec

class LifetimeTableTest extends AnyFlatSpec {

  behavior of "LifetimeTableTest"

  val lft = LifetimeTable() // Table 6.3, serial 3X3 transposer
  lft.addVariable(Lifetime(0,4,"a"))
  lft.addVariable(Lifetime(1,7,"b"))
  lft.addVariable(Lifetime(2,10,"c"))
  lft.addVariable(Lifetime(3,5,"d"))
  lft.addVariable(Lifetime(4,8,"e"))
  lft.addVariable(Lifetime(5,11,"f"))
  lft.addVariable(Lifetime(6,6,"g"))
  lft.addVariable(Lifetime(7,9,"h"))
  lft.addVariable(Lifetime(8,12,"i"))
  lft.setPeriod(9)

  val lftMIMO = LifetimeTable() // parallel 3X3 transposer
  lftMIMO.addVariable(Lifetime(0,2,"a"))
  lftMIMO.addVariable(Lifetime(0,3,"b"))
  lftMIMO.addVariable(Lifetime(0,4,"c"))
  lftMIMO.addVariable(Lifetime(1,2,"d"))
  lftMIMO.addVariable(Lifetime(1,3,"e"))
  lftMIMO.addVariable(Lifetime(1,4,"f"))
  lftMIMO.addVariable(Lifetime(2,2,"g"))
  lftMIMO.addVariable(Lifetime(2,3,"h"))
  lftMIMO.addVariable(Lifetime(2,4,"i"))
  lftMIMO.setPeriod(3)

  it should "getMinimumRegCount" in {

    println(lft.range)
    println(lft.rangeForAnalysis)
    assert(lft.getMinimumRegCount == 4)
    lft.getAllocation

    println(lftMIMO.range)
    println(lftMIMO.rangeForAnalysis)
    assert(lftMIMO.getMinimumRegCount == 6)
    lftMIMO.getAllocation

  }

}
