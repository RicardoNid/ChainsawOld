package xilinx.dsp48e2

object MULTMODE extends Enumeration {
  type MULTMODE = Value
  // postfix expression
  val AB1, AD0B1, AAD01, AD0AD01 = Value
}
import MULTMODE._

class DSPAttrs() {

  // cascading strategy
  var A_INPUT = "DIRECT"
  var B_INPUT = "DIRECT"

  // pre-adder and mult
  var USE_MULT    = "NONE" // "NONE", "MULTIPLY", "DYNAMIC"
  var AMULTSEL    = "A"
  var BMULTSEL    = "B"
  var PREADDINSEL = "A"
  var multMode    = AB1

  // pipeline strategy
  // stage1 & 2
  var AREG, BREG, ACASCREG, BCASCREG = 1 // when set as 1, skip stage
  // stage1 for pipeline matching
  var DREG      = 1
  var INMODEREG = 1
  // stage2 feed the pre-adder
  var ADREG = 1
  // stage3 feed the ALU
  var MREG, CREG                = 1
  var CARRYINREG, CARRYINSELREG = 1
  var OPMODEREG, ALUMODEREG     = 1
  // stage4
  var PREG = 1

  def generics = Seq(
    "A_INPUT" -> A_INPUT,
    "B_INPUT" -> B_INPUT,
    "AREG" -> AREG,
    "BREG" -> BREG,
    "ACASCREG" -> ACASCREG,
    "BCASCREG" -> BCASCREG,
    "DREG" -> DREG,
    "ADREG" -> ADREG,
    "INMODEREG" -> INMODEREG,
    "ALUMODEREG" -> ALUMODEREG,
    "OPMODEREG" -> OPMODEREG,
    "CARRYINREG" -> CARRYINREG,
    "CARRYINSELREG" -> CARRYINSELREG,
    "PREG" -> PREG,
    "AMULTSEL" -> AMULTSEL,
    "BMULTSEL" -> BMULTSEL,
    "PREADDINSEL" -> PREADDINSEL,
    "USE_MULT" -> USE_MULT
  )
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

  def setMult(multMODE: MULTMODE): DSPAttrBuilder = {
    USE_MULT = "MULTIPLY"
    multMODE match {
      case AB1     => AMULTSEL = "A"; BMULTSEL = "B"; PREADDINSEL = "A";
      case AD0B1   => AMULTSEL = "AD"; BMULTSEL = "B"; PREADDINSEL = "A";
      case AAD01   => AMULTSEL = "A"; BMULTSEL = "AD"; PREADDINSEL = "A";
      case AD0AD01 => AMULTSEL = "AD"; BMULTSEL = "AD"; PREADDINSEL = "A";
    }
    multMode = multMODE
    this
  }

  def setALU() = {}

  // FIXME: following strategy is only available for MAC
  def setLatency(latency: Int) = {
    def setStage(stage: Int, set: Boolean) = {
      val value = if (set) 1 else 0
      stage match {
        case 1 =>
          DREG      = value
          INMODEREG = value
        case 2 =>
          ADREG = value
        case 3 =>
          MREG       = value
          CREG       = value
          CARRYINREG = value
          OPMODEREG  = value
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

    latency match {
      case 4 => AREG = 2; ACASCREG = 2; BREG = 2; BCASCREG = 2;
      case 3 => AREG = 1; ACASCREG = 1; BREG = 1; BCASCREG = 1;
      case _ => AREG = 0; ACASCREG = 0; BREG = 0; BCASCREG = 0;
    }

    this
  }

  def build = ret
}

object DSPAttrBuilder {
  def apply(): DSPAttrBuilder = new DSPAttrBuilder()
}
