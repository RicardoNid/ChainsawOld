val signal = true
val cond1 = 1 == 2
val cond2 = 2 == 3
val cond3 = 2 == 2
val result = signal match {
  case cond1 => 1
  case cond2 => 2
  case cond3 => 3
}