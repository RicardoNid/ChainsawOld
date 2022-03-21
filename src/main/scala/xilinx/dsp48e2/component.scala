package xilinx.dsp48e2

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import xilinx.dsp48e2.MULTMODE.AD0B1

// ports for cascading
case class DSPCASC() extends Bundle {
  val A        = SInt(30 bits)
  val B        = SInt(18 bits)
  val P        = SInt(48 bits)
  val CARRY    = Bool()
  val MULTSIGN = Bool()
  val all      = Seq(A, B, P, CARRY, MULTSIGN)
}

case class DSPCONTROL() extends Bundle { // 4
  val ALUMODE    = Bits(4 bits)
  val INMODE     = Bits(5 bits)
  val OPMODE     = Bits(9 bits)
  val CARRYINSEL = Bits(3 bits)
}

case class DSPINPUT() extends Bundle { // 5
  // for a,b,c,d,special strategy is required when unused
  val A       = in SInt (30 bits)
  val B       = in SInt (18 bits)
  val C       = in SInt (48 bits)
  val D       = in SInt (27 bits)
  val CARRYIN = in Bool ()
}

case class DSPOUTPUT() extends Bundle { // 7
  val P                             = out SInt (48 bits)
  val CARRYOUT                      = out SInt (4 bits)
  val XOROUT                        = out Bits (8 bits)
  val OVERFLOW, UNDERFLOW           = out Bool ()
  val PATTERNBDETECT, PATTERNDETECT = out Bool ()
}

case class DSPCEs() extends Bundle { //
  val A1, A2, B1, B2, C, D, AD, M, P, CARRYIN, CTRL, INMODE, ALUMODE = Bool()
  val all                                                            = Seq(A1, A2, B1, B2, C, D, AD, M, P, CARRYIN, CTRL, INMODE, ALUMODE)
  all.foreach(signal => signal.setName("CE" + signal.getPartialName()))
}

case class DSPRSTs() extends Bundle {
  val A, B, C, D, M, P, ALLCARRYIN, CTRL, INMODE, ALUMODE = Bool()
  val all                                                 = Seq(A, B, C, D, M, P, ALLCARRYIN, CTRL, INMODE, ALUMODE)
  all.foreach(signal => signal.setName("RST" + signal.getPartialName()))
}

class DSP48E2(attrs: DSPAttrs) extends BlackBox {

  addGenerics(attrs.generics: _*)
  println(attrs.AMULTSEL)

  val CLK = in Bool ()
  // control
  val INST = in(DSPCONTROL())
  // inputs/outputs for cascading
  val CASCDATAIN  = in(DSPCASC())
  val CASCDATAOUT = out(DSPCASC())
  // ClockEnables & ReSeTs
  val CEs  = in(DSPCEs())
  val RSTs = in(DSPRSTs())
  // inputs/outputs from generic logic
  val DATAIN  = in(DSPINPUT())
  val DATAOUT = out(DSPOUTPUT())

  // set names to be the same as primitive ports
  INST.setName("") // drop the prefix
  DATAIN.setName("")
  DATAOUT.setName("")

  CASCDATAOUT.A.setName("ACOUT")
  CASCDATAOUT.B.setName("BCOUT")
  CASCDATAOUT.P.setName("PCOUT")
  CASCDATAOUT.CARRY.setName("CARRYCASCOUT")
  CASCDATAOUT.MULTSIGN.setName("MULTSIGNOUT")

  CASCDATAIN.A.setName("ACIN")
  CASCDATAIN.B.setName("BCIN")
  CASCDATAIN.P.setName("PCIN")
  CASCDATAIN.CARRY.setName("CARRYCASCIN")
  CASCDATAIN.MULTSIGN.setName("MULTSIGNIN")

  val inputs  = Seq(INST, CASCDATAIN, DATAIN, CEs, RSTs)
  val outputs = Seq(CASCDATAOUT, DATAOUT)

  mapClockDomain(clock = CLK)

  // pre-assign the unused ports
  def preassign() = {
    // deal with CEs and RSTs
    import CEs._
    import attrs._
    val aPipe = AREG max ACASCREG
    val bPipe = BREG max BCASCREG
    aPipe match {
      case 0 => A1 := False; A2 := False
      case 1 => A1 := False; A2 := True
      case 2 => A1 := True; A2 := True
    }
    bPipe match {
      case 0 => CEs.B1 := False; CEs.B2 := False
      case 1 => CEs.B1 := False; CEs.B2 := True
      case 2 => CEs.B1 := True; CEs.B2 := True
    }
    if (CARRYINREG == 1 && CARRYINSELREG == 1) CTRL := True else CTRL := False
    val otherCEs  = Seq(C, D, AD, M, P, CARRYIN, INMODE, ALUMODE)
    val otherREGs = Seq(CREG, DREG, ADREG, MREG, PREG, CARRYINREG, INMODEREG, ALUMODEREG)
    otherREGs.zip(otherCEs).foreach { case (i, bool) => if (i == 1) bool := True else bool := False }
    RSTs.all.foreach(_.clear())

    // deal with unused ports, strategy from UG579 P53 Notes1
    // TODO: find a way to judge unused ports from attrs
    // TODO: implement the followings in the correct way, currently, it only works for DIRECT MODE
    CASCDATAIN.all.foreach(signal => signal := signal.getZero)
  }

  // for symmetric FIR
  // postfix expression, 0 for addition, 1 for multiplication
  // AD0B1C0 means (A + D) * B + C
  def AD0B1C0(A: SInt, B: SInt, C: SInt, D: SInt): SInt = {
    require(attrs.multMode == AD0B1)
    INST.ALUMODE    := B"0000" // result = Z + W + X + Y + CIN
    INST.OPMODE     := B"110000101" // W = C, Z = 0, X,Y = M
    INST.CARRYINSEL := B"000" // CIN = CARRYIN
    INST.INMODE     := B"00000" //

    DATAIN.A       := A
    DATAIN.B       := B
    DATAIN.C       := C
    DATAIN.D       := D
    DATAIN.CARRYIN := False

    DATAOUT.P
  }
}

object SeqMAC {
  def apply(): DSP48E2 = new DSP48E2(new DSPAttrBuilder().build)
}

import xilinx.dsp48e2.MULTMODE._

object SYMFIR {
  def apply(): DSP48E2 = {
    val ret = new DSP48E2(DSPAttrBuilder().setMult(AD0B1).setLatency(4).build)
    ret.preassign()
    ret
  }
}

class DSPDUT extends Component {
  // example of MACC, result = (A+D) * B + C, latency =
  val A = in SInt (30 bits)
  val B = in SInt (18 bits)
  val C = in SInt (48 bits)
  val D = in SInt (27 bits)
  val p = out SInt (48 bits)

  val attr = DSPAttrBuilder().setMult(AD0B1).setLatency(4).build // Multiplier, latency = 4
  val dsp  = SYMFIR()
  p := RegNext(dsp.AD0B1C0(A, B, C, D))
}

object DSPDUT extends App {
  //  GenRTL(new DSPDUT)
  //
  // FIXME: the problem of glbl.gsr and tristate
  // FIXME: the problem of #1
  SimConfig.withWave
    .addRtl("src/main/resources/DSP48E2.v")
    .compile(new DSPDUT)
    .doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
      A #= 13
      B #= 13
      C #= 13
      D #= 13
      sleep(20)
    }

  //  VivadoSynth(new DSPDUT)
}
