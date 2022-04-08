package Chainsaw

import spinal.core._

object ProdWidthMode extends Enumeration {
  type ProdWidthMode = Value
  val SAME, WHOLE = Value
}

import Chainsaw.ProdWidthMode._


/** configuration of complex multiplication
 *
 * @param fast      when set, use 3 DSPs(rather than 4)
 * @param pipeline  number of pipelining stages, i.e. latency
 * @param widthMode strategy of result width
 */
case class ComplexMultConfig(fast: Boolean = true, pipeline: Int = 5, widthMode: ProdWidthMode = WHOLE) {
  require(pipeline >= 3 && pipeline <= 6)
}

case class ComplexMult(type0: HardType[ComplexNumber], type1: HardType[ComplexNumber])
                      (implicit complexMultConfig: ComplexMultConfig)
  extends Component {

  require(complexMultConfig.pipeline >= 3 && complexMultConfig.pipeline <= 6)

  val a = in(type0())
  val b = in(type1())

  val retPeak = a.peak + b.peak + 2
  val retResolution = a.resolution + b.resolution
  val p = out(ComplexNumber(retPeak, retResolution))

  val ar = a.real
  val ai = a.imag
  val br = b.real
  val bi = b.imag

  complexMultConfig.pipeline match {
    case 3 => //
      // mult0, P = ar(br + bi)
      val mid = ((br +^ bi).d * ar.d).d
      // mult1, P + (ai - ar)br
      p.imag := (mid + ((ai -^ ar).d * br.d).d).d
      // mult2, P - (ar + ai)bi
      p.real := (mid - ((ai +^ ar).d * bi.d).d).d
    case 4 =>
      val mid = ((br +^ bi).d * ar.d).d(2)
      p.imag := (mid + ((ai.d -^ ar.d).d * br.d(2)).d).d
      p.real := (mid - ((ai.d +^ ar.d).d * bi.d(2)).d).d
    case 5 =>
      // regs outside dsp
      val aiD1 = ai.d
      val brD1 = br.d
      val biD1 = bi.d
      val mid = ((br.d +^ bi.d).d * ar.d(2)).d(2)
      p.imag := (mid + ((aiD1.d -^ ar.d(2)).d * brD1.d(2)).d).d
      p.real := (mid - ((aiD1.d +^ ar.d(2)).d * biD1.d(2)).d).d
    case 6 =>
      // regs outside dsp
      val arD1 = ar.d
      val aiD2 = ai.d(2)
      val brD2 = br.d(2)
      val biD2 = bi.d(2)
      // dsp operation and regs inside dsp
      val mid = ((br.d +^ bi.d).d * ar.d(2)).d(2)
      p.imag := (mid.d + ((aiD2.d -^ arD1.d(2)).d * brD2.d(2)).d).d
      p.real := (mid.d - ((aiD2.d +^ arD1.d(2)).d * biD2.d(2)).d).d
  }
}
