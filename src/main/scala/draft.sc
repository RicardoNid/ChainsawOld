val temp = (1 to 4).toSeq

val ins = temp.inits.map(_.mkString("")).mkString("\n")

println(ins)