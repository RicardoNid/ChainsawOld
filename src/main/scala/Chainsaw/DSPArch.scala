package Chainsaw

trait DSPArch

sealed trait FIRArch extends DSPArch

object FIRArch {

  case object MAC extends FIRArch

  case object RAG extends FIRArch

  case object DA extends FIRArch

}

sealed trait SAGArch extends DSPArch

object SAGArch {

  case object RAW extends SAGArch // all these

  case object NORMAL extends SAGArch

}