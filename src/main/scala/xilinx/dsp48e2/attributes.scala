package xilinx.dsp48e2

class DSPAttrs() {

  // cascading strategy
  var A_INPUT = "DIRECT"
  var B_INPUT = "DIRECT"

  // pre-adder and mult
  var USE_MULT = "NONE" // "NONE", "MULTIPLY", "DYNAMIC"
  var AMULTSEL = "A"
  var BMULTSEL = "B"
  var PREADDINSEL = "A"

  // pipeline strategy
  // stage1 & 2
  var AREG, BREG, ACASCREG, BCASCREG = 1 // when set as 1, skip stage
  // stage1 for pipeline matching
  var DREG = 1
  var INMODEREG = 1
  // stage2 feed the pre-adder
  var ADREG = 1
  // stage3 feed the ALU
  var MREG, CREG = 1
  var CARRYINREG, CARRYINSELREG = 1
  var OPMODEREG, ALUMODEREG = 1
  // stage4
  var PREG = 1

  def generics = Seq("A_INPUT" -> A_INPUT, "B_INPUT" -> B_INPUT,
    "AREG" -> AREG, "BREG" -> BREG, "ACASCREG" -> ACASCREG, "BCASCREG" -> BCASCREG,
    "DREG" -> DREG, "ADREG" -> ADREG,
    "INMODEREG" -> INMODEREG, "ALUMODEREG" -> ALUMODEREG, "OPMODEREG" -> OPMODEREG,
    "CARRYINREG" -> CARRYINREG, "CARRYINSELREG" -> CARRYINSELREG,
    "PREG" -> PREG)
}

object MULTMODE extends Enumeration {
  type MULTMODE = Value
  // _ means *, AB means A + B, _2 means square, the first is 27 bits, the second is 18 bits
  val A_B, AD_B, A_AD, AD_2 = Value
}

// builder of DSPAttrs
class DSPAttrBuilder {
  val ret = new DSPAttrs

  import ret._

  def setCascaded() = {
    A_INPUT = "CASCADE"
    B_INPUT = "CASCADE"
    this
  }

  import MULTMODE._

  def setMult(multMODE: MULTMODE): DSPAttrBuilder = {
    multMODE match {
      case A_B => AMULTSEL = "A"; BMULTSEL = "B"; PREADDINSEL = "A";
      case AD_B => AMULTSEL = "AD"; BMULTSEL = "B"; PREADDINSEL = "A";
      case A_AD => AMULTSEL = "A"; BMULTSEL = "AD"; PREADDINSEL = "A";
      case AD_2 => AMULTSEL = "AD"; BMULTSEL = "AD"; PREADDINSEL = "A";
    }
    this
  }

  def setALU() = {}

  // following strategy is available for MAC
  def setLatency(latency:Int) = {
    def setStage(stage: Int, set: Boolean) = {
      val value = if (set) 1 else 0
      stage match {
        case 1 =>
          DREG = value
          INMODEREG = value
        case 2 =>
          ADREG = value
        case 3 =>
          MREG = value
          CREG = value
          CARRYINREG = value
          OPMODEREG = value
          ALUMODEREG = value
        case 4 =>
          PREG = value
      }
    }

    val sets = latency match {
      case 0 => Seq()
      case 1 => Seq(3)
      case 2 => Seq(3, 4)
      case 3 => Seq(2, 3, 4)
      case 4 => Seq(1, 2, 3, 4)
    }
    (1 to 4).foreach(i => if (sets.contains(i)) setStage(i, true) else setStage(i, false))
    this
  }

  def build = ret
}

object DSPAttrBuilder {
  def apply(): DSPAttrBuilder = new DSPAttrBuilder()
}