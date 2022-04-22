package Chainsaw.dsl.transform

case class SpaceRepetition(group: Int, step: Int = -1) {
  def expand(size: (Int, Int)) = {
    val (inputSize, outputSize) = size
    if (step == -1) (inputSize * group, outputSize * group)
    else (step * (group - 1) + inputSize, outputSize * group)
  }
}

case class TimeRepetition(group: Int)

case class Repetition(space: Seq[SpaceRepetition], time: TimeRepetition) {

  def spaceFactor = space.map(_.group).product

  def timeFactor = time

  def ⊗(group: Int, step: Int = -1) = {
    if (step == -1) {
      if (space.last.step == -1) Repetition(space.init :+ SpaceRepetition(space.last.group * group), time)
      else Repetition(space :+ SpaceRepetition(group), time)
    }
    else Repetition(space :+ SpaceRepetition(group, step), time)
  }

  def ^(group: Int) = Repetition(space, TimeRepetition(time.group * group))

  def expand(size: (Int,Int)) = {
    var init = size
    space.foreach(rep => init = rep.expand(init))
    init
  }

}

object Repetition {
  def unit = Repetition(Seq(SpaceRepetition(1)), TimeRepetition(1))
}
