package DSP

trait DSPArch

sealed trait FIRArch extends DSPArch

object FIRArch {

  case object MAC extends FIRArch

  case object RAG extends FIRArch

  case object DA extends FIRArch

}

sealed trait SCMArch extends DSPArch

object SCMArch {

  case object CSD extends SCMArch

  case object MULT extends SCMArch

}