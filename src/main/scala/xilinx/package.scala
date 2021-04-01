package object xilinx {

  sealed trait TaskType

  object ELABO extends TaskType

  object SYNTH extends TaskType

  object IMPL extends TaskType

}
